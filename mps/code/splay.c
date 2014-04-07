/* splay.c: SPLAY TREE IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: Splay trees are used to manage potentially unbounded
 * collections of ordered things.  In the MPS these are usually
 * address-ordered memory blocks.
 *
 * .source: <design/splay>
 *
 * .note.stack: It's important that the MPS have a bounded stack
 * size, and this is a problem for tree algorithms.  Basically,
 * we have to avoid recursion.  TODO: Design documentation for this
 * requirement, meanwhile see job003651 and job003640.
 */


#include "splay.h"
#include "mpm.h"

SRCID(splay, "$Id$");


/* SPLAY_DEBUG -- switch for extra debugging
 *
 * Define SPLAY_DEBUG to enable extra consistency checking when modifying
 * splay tree algorithms, which can be tricky to get right.  This will
 * check the tree size and ordering frequently.
 */

/* #define SPLAY_DEBUG */

#define SplayTreeSetRoot(splay, tree) BEGIN ((splay)->root = (tree)); END
#define SplayCompare(tree, key, node) (((tree)->compare)(node, key))
#define SplayHasUpdate(splay) ((splay)->updateNode != SplayTrivUpdate)


/* SplayTreeCheck -- check consistency of SplayTree
 *
 * See guide.impl.c.adt.check and <design/check>.
 */

Bool SplayTreeCheck(SplayTree splay)
{
  UNUSED(splay);
  CHECKS(SplayTree, splay);
  CHECKL(FUNCHECK(splay->compare));
  CHECKL(FUNCHECK(splay->nodeKey));
  CHECKL(FUNCHECK(splay->updateNode));
  /* Can't use CHECKD_NOSIG because TreeEMPTY is NULL. */
  CHECKL(TreeCheck(splay->root));
  return TRUE;
}


/* SplayTreeInit -- initialise a splay tree
 *
 * ``compare`` must provide a total ordering on node keys.
 *
 * ``nodeKey`` extracts a key from a tree node for passing to ``compare``.
 *
 * ``updateNode`` will be applied to nodes from bottom to top when the
 * tree is restructured in order to maintain client properties (see
 * design.mps.splay.prop).  If SplayTrivUpdate is be passed, faster
 * algorithms are chosen for splaying.  Compare SplaySplitDown with
 * SplaySplitRev.
 */

void SplayTreeInit(SplayTree splay,
                   TreeCompare compare,
                   TreeKeyMethod nodeKey,
                   SplayUpdateNodeMethod updateNode)
{
  AVER(splay != NULL);
  AVER(FUNCHECK(compare));
  AVER(FUNCHECK(nodeKey));
  AVER(FUNCHECK(updateNode));

  splay->compare = compare;
  splay->nodeKey = nodeKey;
  splay->updateNode = updateNode;
  SplayTreeSetRoot(splay, TreeEMPTY);
  splay->sig = SplayTreeSig;

  AVERT(SplayTree, splay);
}


/* SplayTreeFinish -- finish a splay tree
 *
 * Does not attempt to descend or finish any tree nodes.
 *
 * TODO: Should probably fail on non-empty tree, so that client code is
 * forced to decide what to do about that.
 */

void SplayTreeFinish(SplayTree splay)
{
  AVERT(SplayTree, splay);
  splay->sig = SigInvalid;
  SplayTreeSetRoot(splay, TreeEMPTY);
  splay->compare = NULL;
  splay->nodeKey = NULL;
  splay->updateNode = NULL;
}


/* SplayTrivUpdate -- trivial update method
 *
 * This is passed to SplayTreeInit to indicate that no client property
 * maintenance is required.  It can also be called to do nothing.
 */

void SplayTrivUpdate(SplayTree splay, Tree tree)
{
  AVERT(SplayTree, splay);
  AVERT(Tree, tree);
}


/* compareLess, compareGreater -- trivial comparisons
 *
 * These comparisons can be passed to SplaySplay to find the leftmost
 * or rightmost nodes in a tree quickly.
 *
 * NOTE: It's also possible to make specialised versions of SplaySplit
 * that traverse left and right unconditionally.  These weren't found
 * to have a significant performance advantage when benchmarking.
 * RB 2014-02-23
 */

static Compare compareLess(Tree tree, TreeKey key)
{
  UNUSED(tree);
  UNUSED(key);
  return CompareLESS;
}

static Compare compareGreater(Tree tree, TreeKey key)
{
  UNUSED(tree);
  UNUSED(key);
  return CompareGREATER;
}


/* SplayDebugUpdate -- force update of client property
 *
 * A debugging utility to recursively update the client property of
 * a subtree.  May not be used in production MPS because it has
 * indefinite stack usage.  See .note.stack.
 */

void SplayDebugUpdate(SplayTree splay, Tree tree)
{
  AVERT(SplayTree, splay);
  AVERT(Tree, tree);
  if (tree == TreeEMPTY)
    return;
  SplayDebugUpdate(splay, TreeLeft(tree));
  SplayDebugUpdate(splay, TreeRight(tree));
  splay->updateNode(splay, tree);
}


/* SplayZig -- move to left child, prepending to right tree
 *
 * Link the top node of the middle tree into the left child of the
 * right tree, then step to the left child.  Returns new middle.
 *
 * See <design/splay/#impl.link.right>.
 *
 *    middle    rightNext            middle
 *      B          E                   A              E
 *     / \        / \          =>                    / \
 *    A   C      D   F                    rightNext D   F
 *          rightFirst                             /
 *                                     rightFirst B
 *                                                 \
 *                                                  C
 */

static Tree SplayZig(Tree middle, Tree *rightFirstIO, Tree *rightNextReturn)
{
  AVERT_CRITICAL(Tree, middle);
  AVER_CRITICAL(rightFirstIO != NULL);
  AVERT_CRITICAL(Tree, *rightFirstIO);
  TreeSetLeft(*rightFirstIO, middle);
  *rightNextReturn = *rightFirstIO;
  *rightFirstIO = middle;
  return TreeLeft(middle);
}

/* SplayZigZig -- move to left child, rotating on on to right tree
 *
 * Rotate the top node of the middle tree over the left child of the
 * right tree, then step to the left child, completing a splay "zig zig"
 * after an initial SplayZig.  Returns new middle.
 *
 *    middle     rightNext           middle       rightNext
 *      B          E                   A              E
 *     / \        / \          =>                    / \
 *    A   C      D   F                   rightFirst B   F
 *          rightFirst                               \
 *                                                    D
 *                                                   /
 *                                                  C
 */

static Tree SplayZigZig(Tree middle, Tree *rightFirstIO, Tree rightNext)
{
  AVERT_CRITICAL(Tree, middle);
  AVER_CRITICAL(rightFirstIO != NULL);
  AVERT_CRITICAL(Tree, *rightFirstIO);
  TreeSetLeft(*rightFirstIO, TreeRight(middle));
  TreeSetRight(middle, *rightFirstIO);
  TreeSetLeft(rightNext, middle);
  *rightFirstIO = middle;
  return TreeLeft(middle);
}
  
/* SplayZag -- mirror image of SplayZig */

static Tree SplayZag(Tree middle, Tree *leftLastIO, Tree *leftPrevReturn)
{
  AVERT_CRITICAL(Tree, middle);
  AVER_CRITICAL(leftLastIO != NULL);
  AVERT_CRITICAL(Tree, *leftLastIO);
  TreeSetRight(*leftLastIO, middle);
  *leftPrevReturn = *leftLastIO;
  *leftLastIO = middle;
  return TreeRight(middle);
}

/* SplayZagZag -- mirror image of SplayZigZig */

static Tree SplayZagZag(Tree middle, Tree *leftLastIO, Tree leftPrev)
{
  AVERT_CRITICAL(Tree, middle);
  AVER_CRITICAL(leftLastIO != NULL);
  AVERT_CRITICAL(Tree, *leftLastIO);
  TreeSetRight(*leftLastIO, TreeLeft(middle));
  TreeSetLeft(middle, *leftLastIO);
  TreeSetRight(leftPrev, middle);
  *leftLastIO = middle;
  return TreeRight(middle);
}


/* SplayState -- the state of splaying between "split" and "assemble"
 *
 * Splaying is divided into two phases: splitting the tree into three,
 * and then assembling a final tree.  This allows for optimisation of
 * certain operations, the key one being SplayTreeNeighbours, which is
 * critical for coalescing memory blocks (see CBSInsert).
 *
 * Note that SplaySplitDown and SplaySplitRev use the trees slightly
 * differently.  SplaySplitRev does not provide "left" and "right", and
 * "leftLast" and "rightFirst" are pointer-reversed spines.
 */

typedef struct SplayStateStruct {
  Tree middle;      /* always non-empty, has the found node at the root */
  Tree left;        /* nodes less than search key during split */
  Tree leftLast;    /* rightmost node on right spine of "left" */
  Tree right;       /* nodes greater than search key during split */
  Tree rightFirst;  /* leftmost node on left spine of "right" */
} SplayStateStruct, *SplayState;


/* SplaySplitDown -- divide the tree around a key
 *
 * Split a tree into three according to a key and a comparison,
 * splaying nested left and right nodes.  Preserves tree ordering.
 * This is a top-down splay procedure, and does not use any recursion
 * or require any parent pointers (see design.mps.impl.top-down).
 *
 * Returns cmp, the relationship of the root of the middle tree to the key,
 * and a SplayState.
 *
 * Does *not* call update to maintain client properties.  See SplaySplitRev.
 */

static Compare SplaySplitDown(SplayStateStruct *stateReturn,
                              SplayTree splay, TreeKey key, TreeCompare compare)
{
  TreeStruct sentinel;
  Tree middle, leftLast, rightFirst, leftPrev, rightNext;
  Compare cmp;

  AVERT(SplayTree, splay);
  AVER(FUNCHECK(compare));
  AVER(!SplayTreeIsEmpty(splay));
  AVER(!SplayHasUpdate(splay));
  
  TreeInit(&sentinel);
  leftLast = &sentinel;
  rightFirst = &sentinel;
  middle = SplayTreeRoot(splay);
  for (;;) {
    cmp = compare(middle, key);
    switch(cmp) {
    default:
      NOTREACHED;
      /* defensive fall-through */
    case CompareEQUAL:
      goto stop;

    case CompareLESS:
      if (!TreeHasLeft(middle))
        goto stop;
      middle = SplayZig(middle, &rightFirst, &rightNext);
      cmp = compare(middle, key);
      switch(cmp) {
      default:
        NOTREACHED;
        /* defensive fall-through */
      case CompareEQUAL:
        goto stop;
      case CompareLESS:
        if (!TreeHasLeft(middle))
          goto stop;
        middle = SplayZigZig(middle, &rightFirst, rightNext);
        break;
      case CompareGREATER:
        if (!TreeHasRight(middle))
          goto stop;
        middle = SplayZag(middle, &leftLast, &leftPrev);
        break;
      }
      break;

    case CompareGREATER:
      if (!TreeHasRight(middle))
        goto stop;
      middle = SplayZag(middle, &leftLast, &leftPrev);
      cmp = compare(middle, key);
      switch(cmp) {
      default:
        NOTREACHED;
        /* defensive fall-through */
      case CompareEQUAL:
        goto stop;
      case CompareGREATER:
        if (!TreeHasRight(middle))
          goto stop;
        middle = SplayZagZag(middle, &leftLast, leftPrev);
        break;
      case CompareLESS:
        if (!TreeHasLeft(middle))
          goto stop;
        middle = SplayZig(middle, &rightFirst, &rightNext);
        break;
      }
      break;
    }
  }

stop:
  stateReturn->middle = middle;
  stateReturn->left = TreeRight(&sentinel);
  stateReturn->leftLast = leftLast == &sentinel ? TreeEMPTY : leftLast;
  stateReturn->right = TreeLeft(&sentinel);
  stateReturn->rightFirst = rightFirst == &sentinel ? TreeEMPTY : rightFirst;
  return cmp;
}


/* SplayAssembleDown -- assemble left right and middle trees into one
 *
 * Takes the result of a SplaySplit and forms a single tree with the
 * root of the middle tree as the root.
 *
 *   left      middle      right                 middle
 *    B          P          V                      P
 *   / \        / \        / \         =>       /     \
 *  A   C      N   Q      U   X               B         V
 *    leftLast       rightFirst              / \       / \
 *                                          A   C     U   X
 *                                               \   /
 *                                                N Q
 *
 * The children of the middle tree are grafted onto the last and first
 * nodes of the side trees, which become the children of the root.
 *
 * Does *not* maintain client properties.  See SplayAssembleRev.
 *
 * See <design/splay/#impl.assemble>.
 */

static void SplayAssembleDown(SplayTree splay, SplayState state)
{
  AVERT(SplayTree, splay);
  AVER(state->middle != TreeEMPTY);
  AVER(!SplayHasUpdate(splay));

  if (state->left != TreeEMPTY) {
    AVER_CRITICAL(state->leftLast != TreeEMPTY);
    TreeSetRight(state->leftLast, TreeLeft(state->middle));
    TreeSetLeft(state->middle, state->left);
  }

  if (state->right != TreeEMPTY) {
    AVER_CRITICAL(state->rightFirst != TreeEMPTY);
    TreeSetLeft(state->rightFirst, TreeRight(state->middle));
    TreeSetRight(state->middle, state->right);
  }
}


/* SplayZigRev -- move to left child, prepending to reversed right tree
 *
 * Same as SplayZig, except that the left spine of the right tree is
 * pointer-reversed, so that its left children point at their parents
 * instead of their children.  This is fixed up in SplayAssembleRev.
 */

static Tree SplayZigRev(Tree middle, Tree *rightFirstIO)
{
  Tree child;
  AVERT_CRITICAL(Tree, middle);
  AVER_CRITICAL(rightFirstIO != NULL);
  AVERT_CRITICAL(Tree, *rightFirstIO);
  child = TreeLeft(middle);
  TreeSetLeft(middle, *rightFirstIO);
  *rightFirstIO = middle;
  return child;
}

/* SplayZigZigRev -- move to left child, rotating onto reversed right tree
 *
 * Same as SplayZigZig, except that the right tree is pointer reversed
 * (see SplayZigRev)
 */

static Tree SplayZigZigRev(Tree middle, Tree *rightFirstIO)
{
  Tree child;
  AVERT_CRITICAL(Tree, middle);
  AVER_CRITICAL(rightFirstIO != NULL);
  AVERT_CRITICAL(Tree, *rightFirstIO);
  child = TreeLeft(middle);
  TreeSetLeft(middle, TreeLeft(*rightFirstIO));
  TreeSetLeft(*rightFirstIO, TreeRight(middle));
  TreeSetRight(middle, *rightFirstIO);
  *rightFirstIO = middle;
  return child;
}

/* SplayZagRev -- mirror image of SplayZigRev */

static Tree SplayZagRev(Tree middle, Tree *leftLastIO)
{
  Tree child;
  AVERT_CRITICAL(Tree, middle);
  AVER_CRITICAL(leftLastIO != NULL);
  AVERT_CRITICAL(Tree, *leftLastIO);
  child = TreeRight(middle);
  TreeSetRight(middle, *leftLastIO);
  *leftLastIO = middle;
  return child;
}

/* SplayZagZagRev -- mirror image of SplayZigZigRev */

static Tree SplayZagZagRev(Tree middle, Tree *leftLastIO)
{
  Tree child;
  AVERT_CRITICAL(Tree, middle);
  AVER_CRITICAL(leftLastIO != NULL);
  AVERT_CRITICAL(Tree, *leftLastIO);
  child = TreeRight(middle);
  TreeSetRight(middle, TreeRight(*leftLastIO));
  TreeSetRight(*leftLastIO, TreeLeft(middle));
  TreeSetLeft(middle, *leftLastIO);
  *leftLastIO = middle;
  return child;
}


/* SplaySplitRev -- divide the tree around a key
 *
 * This is the same as SplaySplit, except that:
 *   - the left and right trees are pointer reversed on their spines
 *   - client properties for rotated nodes (not on the spines) are
 *     updated
 */

static Compare SplaySplitRev(SplayStateStruct *stateReturn,
                             SplayTree splay, TreeKey key, TreeCompare compare)
{
  Tree middle, leftLast, rightFirst;
  Compare cmp;

  AVERT(SplayTree, splay);
  AVER(FUNCHECK(compare));
  AVER(!SplayTreeIsEmpty(splay));
  
  leftLast = TreeEMPTY;
  rightFirst = TreeEMPTY;
  middle = SplayTreeRoot(splay);
  for (;;) {
    cmp = compare(middle, key);
    switch(cmp) {
    default:
      NOTREACHED;
      /* defensive fall-through */
    case CompareEQUAL:
      goto stop;

    case CompareLESS:
      if (!TreeHasLeft(middle))
        goto stop;
      middle = SplayZigRev(middle, &rightFirst);
      cmp = compare(middle, key);
      switch(cmp) {
      default:
        NOTREACHED;
        /* defensive fall-through */
      case CompareEQUAL:
        goto stop;
      case CompareLESS:
        if (!TreeHasLeft(middle))
          goto stop;
        middle = SplayZigZigRev(middle, &rightFirst);
        splay->updateNode(splay, TreeRight(rightFirst));
        break;
      case CompareGREATER:
        if (!TreeHasRight(middle))
          goto stop;
        middle = SplayZagRev(middle, &leftLast);
        break;
      }
      break;

    case CompareGREATER:
      if (!TreeHasRight(middle))
        goto stop;
      middle = SplayZagRev(middle, &leftLast);
      cmp = compare(middle, key);
      switch(cmp) {
      default:
        NOTREACHED;
        /* defensive fall-through */
      case CompareEQUAL:
        goto stop;
      case CompareGREATER:
        if (!TreeHasRight(middle))
          goto stop;
        middle = SplayZagZagRev(middle, &leftLast);
        splay->updateNode(splay, TreeLeft(leftLast));
        break;
      case CompareLESS:
        if (!TreeHasLeft(middle))
          goto stop;
        middle = SplayZigRev(middle, &rightFirst);
        break;
      }
      break;
    }
  }

stop:
  stateReturn->middle = middle;
  stateReturn->leftLast = leftLast;
  stateReturn->rightFirst = rightFirst;
  return cmp;
}


/* SplayUpdateLeftSpine -- undo pointer reversal, updating client property */

static Tree SplayUpdateLeftSpine(SplayTree splay, Tree node, Tree child)
{
  AVERT_CRITICAL(SplayTree, splay);
  AVERT_CRITICAL(Tree, node);
  AVERT_CRITICAL(Tree, child);
  while(node != TreeEMPTY) {
    Tree parent = TreeLeft(node);
    TreeSetLeft(node, child); /* un-reverse pointer */
    splay->updateNode(splay, node);
    child = node;
    node = parent;
  }
  return child;
}

/* SplayUpdateRightSpine -- mirror of SplayUpdateLeftSpine */

static Tree SplayUpdateRightSpine(SplayTree splay, Tree node, Tree child)
{
  AVERT_CRITICAL(SplayTree, splay);
  AVERT_CRITICAL(Tree, node);
  AVERT_CRITICAL(Tree, child);
  while (node != TreeEMPTY) {
    Tree parent = TreeRight(node);
    TreeSetRight(node, child); /* un-reverse pointer */
    splay->updateNode(splay, node);
    child = node;
    node = parent;
  }
  return child;
}


/* SplayAssembleRev -- pointer reversed SplayAssemble
 *
 * Does the same job as SplayAssemble, but operates on pointer-reversed
 * left and right trees, updating client properties.  When we reach
 * this function, the nodes on the spines of the left and right trees
 * will have out of date client properties because their children have
 * been changed by SplaySplitRev.
 */

static void SplayAssembleRev(SplayTree splay, SplayState state)
{
  Tree left, right;

  AVERT(SplayTree, splay);
  AVER(state->middle != TreeEMPTY);
  
  left = TreeLeft(state->middle);
  left = SplayUpdateRightSpine(splay, state->leftLast, left);
  TreeSetLeft(state->middle, left);

  right = TreeRight(state->middle);
  right = SplayUpdateLeftSpine(splay, state->rightFirst, right);
  TreeSetRight(state->middle, right);

  splay->updateNode(splay, state->middle);
}


/* SplaySplit -- call SplaySplitDown or SplaySplitRev as appropriate */

static Compare SplaySplit(SplayStateStruct *stateReturn,
                          SplayTree splay, TreeKey key, TreeCompare compare)
{
  if (SplayHasUpdate(splay))
    return SplaySplitRev(stateReturn, splay, key, compare);
  else
    return SplaySplitDown(stateReturn, splay, key, compare);
}


/* SplayAssemble -- call SplayAssembleDown or SplayAssembleRev as appropriate */

static void SplayAssemble(SplayTree splay, SplayState state)
{
  if (SplayHasUpdate(splay))
    SplayAssembleRev(splay, state);
  else
    SplayAssembleDown(splay, state);
}


/* SplaySplay -- splay the tree around a given key
 *
 * Uses SplaySplitRev/SplayAssembleRev or SplaySplitDown/SplayAssembleDown
 * as appropriate, but also catches the empty tree case and shortcuts
 * the common case where the wanted node is already at the root (due
 * to a previous splay).  The latter shortcut has a significant effect
 * on run time.
 *
 * If a matching node is found, it is splayed to the root and the function
 * returns CompareEQUAL, or if the tree is empty, will also return
 * CompareEQUAL.  Otherwise, CompareGREATER or CompareLESS is returned
 * meaning either the key is greater or less than the new root.  In this
 * case the new root is the last node visited which is either the closest
 * node left or the closest node right of the key.
 *
 * See <design/splay/#impl.splay>.
 */

static Compare SplaySplay(SplayTree splay, TreeKey key, TreeCompare compare)
{
  Compare cmp;
  SplayStateStruct stateStruct;

#ifdef SPLAY_DEBUG
  Count count = TreeDebugCount(SplayTreeRoot(tree), tree->compare, tree->nodeKey);
#endif

  /* Short-circuit common cases.  Splay trees often bring recently
     acccessed nodes to the root. */
  if (SplayTreeIsEmpty(splay) ||
      compare(SplayTreeRoot(splay), key) == CompareEQUAL)
    return CompareEQUAL;

  if (SplayHasUpdate(splay)) {
    cmp = SplaySplitRev(&stateStruct, splay, key, compare);
    SplayAssembleRev(splay, &stateStruct);
  } else {
    cmp = SplaySplitDown(&stateStruct, splay, key, compare);
    SplayAssembleDown(splay, &stateStruct);
  }

  SplayTreeSetRoot(splay, stateStruct.middle);

#ifdef SPLAY_DEBUG
  AVER(count == TreeDebugCount(SplayTreeRoot(tree), tree->compare, tree->nodeKey));
#endif

  return cmp;
}


/* SplayTreeInsert -- insert a node into a splay tree
 *
 *
 * This function is used to insert a node into the tree.  Splays the
 * tree at the node's key.  If an attempt is made to insert a node that
 * compares ``CompareEQUAL`` to an existing node in the tree, then
 * ``FALSE`` will be returned and the node will not be inserted.
 *
 * NOTE: It would be possible to use split here, then assemble around
 * the new node, leaving the neighbour where it was, but it's probably
 * a good thing for key neighbours to be tree neighbours.
 */

Bool SplayTreeInsert(SplayTree splay, Tree node) {
  Tree neighbour;

  AVERT(SplayTree, splay);
  AVERT(Tree, node);
  AVER(TreeLeft(node) == TreeEMPTY);
  AVER(TreeRight(node) == TreeEMPTY);

  if (SplayTreeIsEmpty(splay)) {
    SplayTreeSetRoot(splay, node);
    return TRUE;
  }
  
  switch (SplaySplay(splay, splay->nodeKey(node), splay->compare)) {
  default:
    NOTREACHED;
    /* defensive fall-through */
  case CompareEQUAL: /* duplicate node */
    return FALSE;
    
  case CompareGREATER: /* left neighbour is at root */
    neighbour = SplayTreeRoot(splay);
    SplayTreeSetRoot(splay, node);
    TreeSetRight(node, TreeRight(neighbour));
    TreeSetLeft(node, neighbour);
    TreeSetRight(neighbour, TreeEMPTY);
    break;

  case CompareLESS: /* right neighbour is at root */
    neighbour = SplayTreeRoot(splay);
    SplayTreeSetRoot(splay, node);
    TreeSetLeft(node, TreeLeft(neighbour));
    TreeSetRight(node, neighbour);
    TreeSetLeft(neighbour, TreeEMPTY);
    break;
  }

  splay->updateNode(splay, neighbour);
  splay->updateNode(splay, node);
  return TRUE;
}


/* SplayTreeDelete -- delete a node from a splay tree
 *
 * Delete a node from the tree.  If the tree does not contain the given
 * node then ``FALSE`` will be returned.  The client must not pass a
 * node whose key compares equal to a different node in the tree.
 *
 * The function first splays the tree at the given key.
 *
 * TODO: If the node has zero or one children, then the replacement
 * would be the leftLast or rightFirst after a SplaySplit, and would
 * avoid a search for a replacement in more cases.
 */

Bool SplayTreeDelete(SplayTree splay, Tree node) {
  Tree leftLast;
  Compare cmp;

  AVERT(SplayTree, splay);
  AVERT(Tree, node);

  if (SplayTreeIsEmpty(splay))
    return FALSE;

  cmp = SplaySplay(splay, splay->nodeKey(node), splay->compare);
  AVER(cmp != CompareEQUAL || SplayTreeRoot(splay) == node);

  if (cmp != CompareEQUAL) {
    return FALSE;
  } else if (!TreeHasLeft(node)) {
    SplayTreeSetRoot(splay, TreeRight(node));
    TreeClearRight(node);
  } else if (!TreeHasRight(node)) {
    SplayTreeSetRoot(splay, TreeLeft(node));
    TreeClearLeft(node);
  } else {
    Tree rightHalf = TreeRight(node);
    TreeClearRight(node);
    SplayTreeSetRoot(splay, TreeLeft(node));
    TreeClearLeft(node);
    (void)SplaySplay(splay, NULL, compareGreater);
    leftLast = SplayTreeRoot(splay);
    AVER(leftLast != TreeEMPTY);
    AVER(!TreeHasRight(leftLast));
    TreeSetRight(leftLast, rightHalf);
    splay->updateNode(splay, leftLast);
  }

  TreeFinish(node);

  return TRUE;
}


/* SplayTreeFind -- search for a node in a splay tree matching a key
 *
 * Search the tree for a node that compares ``CompareEQUAL`` to a key
 * Splays the tree at the key.  Returns ``FALSE`` if there is no such
 * node in the tree, otherwise ``*nodeReturn`` will be set to the node.
 */

Bool SplayTreeFind(Tree *nodeReturn, SplayTree splay, TreeKey key) {
  AVERT(SplayTree, splay);
  AVER(nodeReturn != NULL);

  if (SplayTreeIsEmpty(splay))
    return FALSE;

  if (SplaySplay(splay, key, splay->compare) != CompareEQUAL)
    return FALSE;

  *nodeReturn = SplayTreeRoot(splay);
  return TRUE;
}


/* SplayTreeSuccessor -- splays a tree at the root's successor
 *
 * Must not be called on en empty tree.  Successor need not exist,
 * in which case TreeEMPTY is returned, and the tree is unchanged.
 */

static Tree SplayTreeSuccessor(SplayTree splay) {
  Tree oldRoot, newRoot;

  AVERT(SplayTree, splay);
  AVER(!SplayTreeIsEmpty(splay));

  oldRoot = SplayTreeRoot(splay);

  if (!TreeHasRight(oldRoot))
    return TreeEMPTY; /* No successor */

  /* temporarily chop off the left half-tree, inclusive of root */
  SplayTreeSetRoot(splay, TreeRight(oldRoot));
  TreeSetRight(oldRoot, TreeEMPTY);
  (void)SplaySplay(splay, NULL, compareLess);
  newRoot = SplayTreeRoot(splay);
  AVER(newRoot != TreeEMPTY);
  AVER(TreeLeft(newRoot) == TreeEMPTY);
  TreeSetLeft(newRoot, oldRoot);
  splay->updateNode(splay, oldRoot);
  splay->updateNode(splay, newRoot);

  return newRoot;
}


/* SplayTreeNeighbours
 *
 * Search for the two nodes in a splay tree neighbouring a key.
 * Splays the tree at the key. ``*leftReturn`` will be the neighbour
 * which compares less than the key if such a neighbour exists; otherwise
 * it will be ``TreeEMPTY``. ``*rightReturn`` will be the neighbour which
 * compares greater than the key if such a neighbour exists; otherwise
 * it will be ``TreeEMPTY``. The function returns ``FALSE`` if any node
 * in the tree compares ``CompareEQUAL`` with the given key.
 *
 * TODO: Change to SplayTreeCoalesce that takes a function that can
 * direct the deletion of one of the neighbours, since this is a
 * good moment to do it, avoiding another search and splay.
 *
 * This implementation uses SplaySplit to find both neighbours in a
 * single splay (see design.mps.splay.impl.neighbours).
 */

Bool SplayTreeNeighbours(Tree *leftReturn, Tree *rightReturn,
                         SplayTree splay, TreeKey key)
{
  SplayStateStruct stateStruct;
  Bool found;
  Compare cmp;
#ifdef SPLAY_DEBUG
  Count count = TreeDebugCount(SplayTreeRoot(tree), tree->compare, tree->nodeKey);
#endif


  AVERT(SplayTree, splay);
  AVER(leftReturn != NULL);
  AVER(rightReturn != NULL);

  if (SplayTreeIsEmpty(splay)) {
    *leftReturn = *rightReturn = TreeEMPTY;
    return TRUE;
  }

  cmp = SplaySplit(&stateStruct, splay, key, splay->compare);

  switch (cmp) {
  default:
    NOTREACHED;
    /* defensive fall-through */
  case CompareEQUAL:
    found = FALSE;
    break;

  case CompareLESS:
    AVER(!TreeHasLeft(stateStruct.middle));
    *rightReturn = stateStruct.middle;
    *leftReturn = stateStruct.leftLast;
    found = TRUE;
    break;

  case CompareGREATER:
    AVER(!TreeHasRight(stateStruct.middle));
    *leftReturn = stateStruct.middle;
    *rightReturn = stateStruct.rightFirst;
    found = TRUE;
    break;
  }

  SplayAssemble(splay, &stateStruct);
  SplayTreeSetRoot(splay, stateStruct.middle);

#ifdef SPLAY_DEBUG
  AVER(count == TreeDebugCount(SplayTreeRoot(tree), tree->compare, tree->nodeKey));
#endif

  return found;
}


/* SplayTreeFirst, SplayTreeNext -- iterators
 *
 * SplayTreeFirst returns TreeEMPTY if the tree is empty. Otherwise,
 * it splays the tree to the first node, and returns the new root.
 *
 * SplayTreeNext takes a tree and splays it to the successor of a key
 * and returns the new root. Returns TreeEMPTY is there are no
 * successors.
 *
 * SplayTreeFirst and SplayTreeNext do not require the tree to remain
 * unmodified.
 *
 * IMPORTANT: Iterating over the tree using these functions will leave
 * the tree totally unbalanced, throwing away optimisations of the tree
 * shape caused by previous splays.  Consider using TreeTraverse instead.
 */

Tree SplayTreeFirst(SplayTree splay) {
  Tree node;

  AVERT(SplayTree, splay);

  if (SplayTreeIsEmpty(splay))
    return TreeEMPTY;

  (void)SplaySplay(splay, NULL, compareLess);
  node = SplayTreeRoot(splay);
  AVER(node != TreeEMPTY);
  AVER(TreeLeft(node) == TreeEMPTY);

  return node;
}

Tree SplayTreeNext(SplayTree splay, TreeKey oldKey) {
  AVERT(SplayTree, splay);

  if (SplayTreeIsEmpty(splay))
    return TreeEMPTY;
  
  /* Make old node the root.  Probably already is.  We don't mind if the
     node has been deleted, or replaced by a node with the same key. */
  switch (SplaySplay(splay, oldKey, splay->compare)) {
  default:
    NOTREACHED;
    /* defensive fall-through */
  case CompareGREATER:
    return SplayTreeRoot(splay);

  case CompareLESS:
  case CompareEQUAL:
    return SplayTreeSuccessor(splay);
  }
}


/* SplayNodeDescribe -- Describe a node in the splay tree
 *
 * Note that this breaks the restriction of .note.stack.
 * This is alright as the function is debug only.
 */

static Res SplayNodeDescribe(Tree node, mps_lib_FILE *stream,
                             TreeDescribeMethod nodeDescribe) {
  Res res;

#if defined(AVER_AND_CHECK)
  if (!TreeCheck(node)) return ResFAIL;
  /* stream and nodeDescribe checked by SplayTreeDescribe */
#endif

  res = WriteF(stream, "( ", NULL);
  if (res != ResOK) return res;

  if (TreeHasLeft(node)) {
    res = SplayNodeDescribe(TreeLeft(node), stream, nodeDescribe);
    if (res != ResOK) return res;

    res = WriteF(stream, " / ", NULL);
    if (res != ResOK) return res;
  }

  res = (*nodeDescribe)(node, stream);
  if (res != ResOK) return res;

  if (TreeHasRight(node)) {
    res = WriteF(stream, " \\ ", NULL);
    if (res != ResOK) return res;

    res = SplayNodeDescribe(TreeRight(node), stream, nodeDescribe);
    if (res != ResOK) return res;
  }

  res = WriteF(stream, " )", NULL);
  if (res != ResOK) return res;

  return ResOK;
}


/* SplayFindFirstCompare, SplayFindLastCompare -- filtering searches
 *
 * These are used by SplayFindFirst and SplayFindLast as comparison
 * functions to SplaySplit in order to home in on a node using client
 * tests.  The way to understand them is that the comparison values
 * they return have nothing to do with the tree ordering, but are instead
 * like commands that tell SplaySplit whether to "go left", "stop", or
 * "go right" according to the results of testNode and testTree.
 * Since splaying preserves the order of the tree, any tests can be
 * applied to navigate to a destination.
 *
 * In the MPS these are mainly used by the CBS to search for memory
 * blocks above a certain size.  Their performance is quite critical.
 */

typedef struct SplayFindClosureStruct {
  SplayTestNodeMethod testNode;
  SplayTestTreeMethod testTree;
  void *p;
  Size s;
  SplayTree splay;
  Bool found;
} SplayFindClosureStruct, *SplayFindClosure;

static Compare SplayFindFirstCompare(Tree node, TreeKey key)
{
  SplayFindClosure closure;
  void *closureP;
  Size closureS;
  SplayTestNodeMethod testNode;
  SplayTestTreeMethod testTree;
  SplayTree splay;

  AVERT(Tree, node);
  AVER(key != NULL);

  /* Lift closure values into variables so that they aren't aliased by
     calls to the test functions. */
  closure = (SplayFindClosure)key;
  closureP = closure->p;
  closureS = closure->s;
  testNode = closure->testNode;
  testTree = closure->testTree;
  splay = closure->splay;
  
  if (TreeHasLeft(node) &&
     (*testTree)(splay, TreeLeft(node), closureP, closureS)) {
    return CompareLESS;
  } else if ((*testNode)(splay, node, closureP, closureS)) {
    closure->found = TRUE;
    return CompareEQUAL;
  } else {
    /* If there's a right subtree but it doesn't satisfy the tree test
       then we want to terminate the splay right now.  SplaySplay will
       return TRUE, so the caller must check closure->found to find out
       whether the result node actually satisfies testNode. */
    if (TreeHasRight(node) &&
        !(*testTree)(splay, TreeRight(node), closureP, closureS)) {
      closure->found = FALSE;
      return CompareEQUAL;
    }
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
  SplayTree splay;

  AVERT(Tree, node);
  AVER(key != NULL);

  /* Lift closure values into variables so that they aren't aliased by
     calls to the test functions. */
  closure = (SplayFindClosure)key;
  closureP = closure->p;
  closureS = closure->s;
  testNode = closure->testNode;
  testTree = closure->testTree;
  splay = closure->splay;

  if (TreeHasRight(node) &&
     (*testTree)(splay, TreeRight(node), closureP, closureS)) {
     return CompareGREATER;
  } else if ((*testNode)(splay, node, closureP, closureS)) {
    closure->found = TRUE;
    return CompareEQUAL;
  } else {
    /* See SplayFindFirstCompare. */
    if (TreeHasLeft(node) &&
        !(*testTree)(splay, TreeLeft(node), closureP, closureS)) {
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
 * If there is no satisfactory node, ``FALSE`` is returned, otherwise
 * ``*nodeReturn`` is set to the node.
 *
 * The given callbacks testNode and testTree detect this property in
 * a single node or a sub-tree rooted at a node, and both receive the
 * arbitrary closures closureP and closureS.
 *
 * TODO: This repeatedly splays failed matches to the root and rotates
 * them, so it could have quite an unbalancing effect if size is small.
 * Think about a better search, perhaps using TreeTraverse?
 */

Bool SplayFindFirst(Tree *nodeReturn, SplayTree splay,
                    SplayTestNodeMethod testNode,
                    SplayTestTreeMethod testTree,
                    void *closureP, Size closureS)
{
  SplayFindClosureStruct closureStruct;
  Bool found;

  AVER(nodeReturn != NULL);
  AVERT(SplayTree, splay);
  AVER(FUNCHECK(testNode));
  AVER(FUNCHECK(testTree));

  if (SplayTreeIsEmpty(splay) ||
      !testTree(splay, SplayTreeRoot(splay), closureP, closureS))
    return FALSE; /* no suitable nodes in tree */

  closureStruct.p = closureP;
  closureStruct.s = closureS;
  closureStruct.testNode = testNode;
  closureStruct.testTree = testTree;
  closureStruct.splay = splay;
  closureStruct.found = FALSE;

  found = SplaySplay(splay, &closureStruct,
                     SplayFindFirstCompare) == CompareEQUAL &&
          closureStruct.found;

  while (!found) {
    Tree oldRoot, newRoot;
    
    /* FIXME: Rename to "seen" and "not yet seen" or something. */
    oldRoot = SplayTreeRoot(splay);
    newRoot = TreeRight(oldRoot);

    if (newRoot == TreeEMPTY || !(*testTree)(splay, newRoot, closureP, closureS))
      return FALSE; /* no suitable nodes in the rest of the tree */
  
    /* Temporarily chop off the left half-tree, inclusive of root,
       so that the search excludes any nodes we've seen already. */
    SplayTreeSetRoot(splay, newRoot);
    TreeSetRight(oldRoot, TreeEMPTY);

    found = SplaySplay(splay, &closureStruct,
                       SplayFindFirstCompare) == CompareEQUAL &&
            closureStruct.found;

    /* Restore the left tree, then rotate left so that the node we
       just splayed is at the root.  Update both. */
    newRoot = SplayTreeRoot(splay);
    TreeSetRight(oldRoot, newRoot);
    SplayTreeSetRoot(splay, oldRoot);
    TreeRotateLeft(&splay->root);
    splay->updateNode(splay, oldRoot);
    splay->updateNode(splay, newRoot);
  }

  *nodeReturn = SplayTreeRoot(splay);
  return TRUE;
}


/* SplayFindLast -- As SplayFindFirst but in reverse address order */

Bool SplayFindLast(Tree *nodeReturn, SplayTree splay,
                   SplayTestNodeMethod testNode,
                   SplayTestTreeMethod testTree,
                   void *closureP, Size closureS)
{
  SplayFindClosureStruct closureStruct;
  Bool found;

  AVER(nodeReturn != NULL);
  AVERT(SplayTree, splay);
  AVER(FUNCHECK(testNode));
  AVER(FUNCHECK(testTree));

  if (SplayTreeIsEmpty(splay) ||
      !testTree(splay, SplayTreeRoot(splay), closureP, closureS))
    return FALSE; /* no suitable nodes in tree */

  closureStruct.p = closureP;
  closureStruct.s = closureS;
  closureStruct.testNode = testNode;
  closureStruct.testTree = testTree;
  closureStruct.splay = splay;

  found = SplaySplay(splay, &closureStruct,
                     SplayFindLastCompare) == CompareEQUAL &&
          closureStruct.found;

  while (!found) {
    Tree oldRoot, newRoot;
    
    oldRoot = SplayTreeRoot(splay);
    newRoot = TreeLeft(oldRoot);

    if (newRoot == TreeEMPTY || !(*testTree)(splay, newRoot, closureP, closureS))
      return FALSE; /* no suitable nodes in the rest of the tree */
  
    /* Temporarily chop off the right half-tree, inclusive of root,
       so that the search excludes any nodes we've seen already. */
    SplayTreeSetRoot(splay, newRoot);
    TreeSetLeft(oldRoot, TreeEMPTY);

    found = SplaySplay(splay, &closureStruct,
                       SplayFindLastCompare) == CompareEQUAL &&
            closureStruct.found;

    /* Restore the right tree, then rotate right so that the node we
       just splayed is at the root.  Update both. */
    newRoot = SplayTreeRoot(splay);
    TreeSetLeft(oldRoot, newRoot);
    SplayTreeSetRoot(splay, oldRoot);
    TreeRotateRight(&splay->root);
    splay->updateNode(splay, oldRoot);
    splay->updateNode(splay, newRoot);
  }

  *nodeReturn = SplayTreeRoot(splay);
  return TRUE;
}


/* SplayNodeRefresh -- updates the client property that has changed at a node
 *
 * This function undertakes to call the client updateNode callback for each
 * node affected by the change in properties at the given node (which has
 * the given key) in an appropriate order.
 *
 * The function fullfils its job by first splaying at the given node, and
 * updating the single node.  In the MPS it is used by the CBS during
 * coalescing, when the node is likely to be at (or adjacent to) the top
 * of the tree anyway.
 */

void SplayNodeRefresh(SplayTree splay, Tree node)
{
  Compare cmp;

  AVERT(SplayTree, splay);
  AVERT(Tree, node);
  AVER(!SplayTreeIsEmpty(splay)); /* must contain node, at least */
  AVER(SplayHasUpdate(splay)); /* otherwise, why call? */

  cmp = SplaySplay(splay, splay->nodeKey(node), splay->compare);
  AVER(cmp == CompareEQUAL);
  AVER(SplayTreeRoot(splay) == node);

  splay->updateNode(splay, node);
}


/* SplayNodeUpdate -- update the client property without splaying */

void SplayNodeUpdate(SplayTree splay, Tree node)
{
  AVERT(SplayTree, splay);
  AVERT(Tree, node);
  AVER(!TreeHasLeft(node)); /* otherwise, call SplayNodeRefresh */
  AVER(!TreeHasRight(node)); /* otherwise, call SplayNodeRefresh */
  AVER(SplayHasUpdate(splay)); /* otherwise, why call? */

  splay->updateNode(splay, node);
}


/* SplayTreeDescribe -- Describe a splay tree
 *
 * See <design/splay/#function.splay.tree.describe>.
 */

Res SplayTreeDescribe(SplayTree splay, mps_lib_FILE *stream,
                      TreeDescribeMethod nodeDescribe) {
  Res res;

#if defined(AVER_AND_CHECK)
  if (!SplayTreeCheck(splay)) return ResFAIL;
  if (stream == NULL) return ResFAIL;
  if (!FUNCHECK(nodeDescribe)) return ResFAIL;
#endif

  res = WriteF(stream,
               "Splay $P {\n", (WriteFP)splay,
               "  compare $F\n", (WriteFF)splay->compare,
               NULL);
  if (res != ResOK) return res;

  if (SplayTreeRoot(splay) != TreeEMPTY) {
    res = SplayNodeDescribe(SplayTreeRoot(splay), stream, nodeDescribe);
    if (res != ResOK) return res;
  }

  res = WriteF(stream, "\n}\n", NULL);
  return res;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
