/* tree.h: BINARY TREE HEADER
 *
 * $Id$
 * Copyright (C) 2014 Ravenbrook Limited.  See end of file for license.
 *
 * Simple binary trees with utilities, for use as building blocks.
 * Keep it simple, like Rings (see ring.h).
 */

#ifndef tree_h
#define tree_h

#include "check.h"
#include "mpmtypes.h"


/* TreeStruct -- binary tree structure
 *
 * The tree structure is used in a field in other structures in order
 * to link them together in a binary tree.
 */

typedef struct TreeStruct *Tree;
typedef struct TreeStruct {
  Tree left, right;
} TreeStruct;


/* TreeKey and TreeCompare -- ordered binary trees
 *
 * Binary trees are almost always ordered, and these types provide the
 * abstraction for ordering.  A TreeCompare method returns whether a key
 * is less than, equal to, or greater than the key in a tree node.  A
 * TreeKeyMethod extracts a key from a node, depending on how TreeStruct
 * is embedded within its parent structure.
 */

typedef void *TreeKey;
typedef Compare (*TreeCompare)(Tree tree, TreeKey key);
typedef TreeKey (*TreeKeyMethod)(Tree tree);


/* When storing Addrs in a tree, it is fastest to cast the Addr
 * directly to a TreeKey. This assumes that Addr and TreeKey are
 * compatible, possibly breaking <design/type/#addr.use>. On an exotic
 * platform where the types are not convertible, take the address of
 * the variable in TreeKeyOfAddrVar, and dereference the address in
 * AddrOfTreeKey.
 */
#define TreeKeyOfAddrVar(var) ((TreeKey)(var))
#define AddrOfTreeKey(key) ((Addr)(key))


/* TreeEMPTY -- the empty tree
 *
 * TreeEMPTY is the tree with no nodes, and hence unable to satisfy its
 * olfactory senses.  Empty trees should not be represented with NULL,
 * as this is ambiguous.  However, TreeEMPTY is in fact a null pointer for
 * performance.  To check whether you have it right, try temporarily
 * defining TreeEMPTY to (Tree)1 or similar.
 */

#define TreeEMPTY ((Tree)0)

extern Bool TreeCheck(Tree tree);
extern Bool TreeCheckLeaf(Tree tree);
extern Count TreeDebugCount(Tree tree, TreeCompare compare, TreeKeyMethod key);

#define TreeInit(tree) \
  BEGIN \
    Tree _tree = (tree); \
    AVER(_tree != NULL); \
    _tree->left = TreeEMPTY; \
    _tree->right = TreeEMPTY; \
    AVERT(Tree, _tree); \
  END

#define TreeFinish(tree) \
  BEGIN \
    Tree _tree = (tree); \
    AVERT(Tree, _tree); \
  END

#define TREE_ELT(type, field, node) \
  PARENT(type ## Struct, field, node)

#define TreeLeft(tree) RVALUE((tree)->left)

#define TreeRight(tree) RVALUE((tree)->right)

#define TreeSetLeft(tree, child) \
  BEGIN \
    (tree)->left = (child); \
  END

#define TreeSetRight(tree, child) \
  BEGIN \
    (tree)->right = (child); \
  END

#define TreeClearLeft(tree) \
  BEGIN \
    (tree)->left = TreeEMPTY; \
  END

#define TreeClearRight(tree) \
  BEGIN \
    (tree)->right = TreeEMPTY; \
  END

#define TreeHasLeft(tree) (TreeLeft(tree) != TreeEMPTY)
#define TreeHasRight(tree) (TreeRight(tree) != TreeEMPTY)

extern Compare TreeFind(Tree *treeReturn, Tree root,
                        TreeKey key, TreeCompare compare);
extern Bool TreeFindNext(Tree *treeReturn, Tree root,
                         TreeKey key, TreeCompare compare);

extern Bool TreeInsert(Tree *treeReturn, Tree root, Tree node,
                       TreeKey key, TreeCompare compare);

typedef Bool TreeVisitor(Tree tree, void *closureP, Size closureS);
extern Bool TreeTraverse(Tree tree,
                         TreeCompare compare,
                         TreeKeyMethod key,
                         TreeVisitor visit, void *closureP, Size closureS);
extern Bool TreeTraverseMorris(Tree tree, TreeVisitor visit,
                               void *closureP, Size closureS);

extern void TreeRotateLeft(Tree *nodeIO);
extern void TreeRotateRight(Tree *nodeIO);
extern Tree TreeReverseLeftSpine(Tree tree);
extern Tree TreeReverseRightSpine(Tree tree);
extern Count TreeToVine(Tree *treeIO);
extern void TreeBalance(Tree *treeIO);

extern void TreeTraverseAndDelete(Tree *treeIO, TreeVisitor visitor,
                                  void *closureP, Size closureS);

#endif /* tree_h */

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
