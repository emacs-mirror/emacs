/* This file implements an efficient interval data-structure.

Copyright (C) 2017-2022  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>
#include <math.h>

#include "itree.h"

/*
   Intervals of the form [BEGIN, END), are stored as nodes inside a RB
   tree, ordered by BEGIN.  The core operation of this tree (besides
   insert, remove, etc.) is finding all intervals intersecting with
   some given interval.  In order to perform this operation
   efficiently, every node stores a third value called LIMIT. (See
   https://en.wikipedia.org/wiki/Interval_tree#Augmented_tree and its
   source Introduction to Algorithms, Cormen et al. .)

   ==== Finding intervals ====

   If we search for all intervals intersecting with (X, Y], we look at
   some node and test whether

   NODE.BEGIN > Y

   Due to the invariant of the search tree, we know, that we may
   safely prune NODE's right subtree if this test succeeds, since all
   intervals begin strictly after Y.

   But we can not make such an assumptions about the left tree, since
   all we know is that the intervals in this subtree must start before
   or at NODE.BEGIN.  So we can't tell, whether they end before X or
   not.  To solve this problem we add another attribute to each node,
   called LIMIT.

   The LIMIT of a node is the largest END value occurring in the nodes
   subtree (including the node itself).  Thus, we may look at the left
   child of some NODE and test whether

   NODE.left.LIMIT < X

   and this tells us, if all intervals in the left subtree of NODE end
   before X and if they can be pruned.

   Conversely, if this inequality is false, the left subtree must
   contain at least one intersecting interval, giving a resulting time
   complexity of O(K*log(N)) for this operation, where K is the size
   of the result set and N the size of the tree.

   ==== FIXME: bug#58342 some important operations remain slow ===

   The amortized costs of Emacs' previous-overlay-change and
   next-overlay-change functions are O(N) with this data structure.
   The root problem is that we only have an order for the BEG field,
   but not the END.  The previous/next overlay change operations need
   to find the nearest point where there is *either* an interval BEG
   or END point, but there is no efficient way to narrow the search
   space over END positions.

   Consider the case where next-overlay-change is called at POS, all
   interval BEG positions are less than pos POS and all interval END
   posistions are after.  These END positions have no order, and so
   *every* interval must be examined.  This is at least O(N).  The
   previous-overlay-change case is similar.  The root issue is that
   the iterative "narrowing" approach is not guaranteed to reduce the
   search space in logarithmic time, since END is not ordered in the
   tree.

   One might argue that the LIMIT value will do this narrowing, but
   this narrowing is O(K*log(N)) where K is the size of the result
   set.  If we are interested in finding the node in a range with the
   smallest END, we might have to examine all K nodes in that range.
   In the case of the *-overlay-channge functions, K may well be equal
   to N.

   Ideally, a tree based data structure for overlays would have
   O(log(N)) performance for previous-overlay-change and
   next-overlay-change, as these are called in performance sensitive
   situations such as redisplay.  The only way I can think of
   achieving this is by keeping one ordering by BEG and a separate
   ordering by END, and then performing logic quite similar to the
   current Emacs overlays-before and overlays-after lists.

   ==== Adjusting intervals ====

   Since this data-structure will be used for overlays in an Emacs
   buffer, a second core operation is the ability to insert and delete
   gaps in the tree.  This models the insertion and deletion of text
   in a buffer and the effects it may have on the positions of
   overlays.

   Consider this: Something gets inserted at position P into a buffer
   and assume that all overlays occur strictly after P.  Ordinarily,
   we would have to iterate all overlays and increment their BEGIN and
   END values accordingly (the insertion of text pushes them back).
   In order to avoid this, we introduce yet another node attribute,
   called OFFSET.

   The OFFSET of some some subtree, represented by its root, is the
   amount of shift that needs to be applied to its BEGIN, END and
   LIMIT values, in order to get to the actual buffer positions.
   Coming back to the example, all we would need to do in this case,
   is to increment the OFFSET of the tree's root, without any
   traversal of the tree itself.

   As a consequence, the real values of BEGIN, END and LIMIT of some
   NODE need to be computed by incrementing them by the sum of NODE's
   OFFSET and all of its ancestors offsets.  Therefore, we store a
   counter (otick) inside every node and also the tree, by which we
   remember the fact, that a node's path to the root has no offsets
   applied (i.e. its values are up to date).  This is the case if some
   node's value differs from the tree's one, the later of which is
   incremented whenever some node's offset has changed.  */

/* +=======================================================================+
 * | Stack
 * +=======================================================================+ */

typedef uintptr_t nodeptr_and_flag;

static inline nodeptr_and_flag
make_nav (struct itree_node *ptr, bool flag)
{
  uintptr_t v = (uintptr_t) ptr;
  /* We assume alignment imposes the LSB is clear for us to use it.  */
  eassert (!(v & 1));
  return v | !!flag;
}

static inline struct itree_node *
nav_nodeptr (nodeptr_and_flag nav)
{
  return (struct itree_node *) (nav & (~(uintptr_t)1));
}

static inline bool
nav_flag (nodeptr_and_flag nav)
{
  return (bool) (nav & 1);
}

/* Simple dynamic array. */
struct interval_stack
{
  nodeptr_and_flag *nodes;
  size_t size;
  size_t length;
};

/* This is just a simple dynamic array with stack semantics. */

static struct interval_stack*
interval_stack_create (intmax_t initial_size)
{
  struct interval_stack *stack = xmalloc (sizeof (struct interval_stack));
  stack->size = max (0, initial_size);
  stack->nodes = xmalloc (stack->size * sizeof (struct itree_node*));
  stack->length = 0;
  return stack;
}

static void
interval_stack_destroy (struct interval_stack *stack)
{
  if (! stack)
    return;
  if (stack->nodes)
    xfree (stack->nodes);
  xfree (stack);
}

static void
interval_stack_clear (struct interval_stack *stack)
{
  stack->length = 0;
}

static inline void
interval_stack_ensure_space (struct interval_stack *stack, uintmax_t nelements)
{
  if (nelements > stack->size)
    {
      stack->size = (nelements + 1) * 2;
      stack->nodes = xrealloc (stack->nodes,
			       stack->size * sizeof (*stack->nodes));
    }
}

/* Push NODE on the STACK, while settings its visited flag to FLAG. */

static inline void
interval_stack_push_flagged (struct interval_stack *stack,
			     struct itree_node *node, bool flag)
{
  eassert (node);

  /* FIXME: While the stack used in the iterator is bounded by the tree
     depth and could be easily pre-allocated to a large enough size to avoid
     this "ensure" check, `interval_stack_push` is also used elsewhere to
     simply collect some subset of the overlays, where it's only bounded by
     the total number of overlays in the buffer (which can be large and thus
     preferably not pre-allocated needlessly).  */
  interval_stack_ensure_space (stack, stack->length + 1);

  stack->nodes[stack->length] = make_nav (node, flag);
  stack->length++;
}

static inline void
interval_stack_push (struct interval_stack *stack, struct itree_node *node)
{
  interval_stack_push_flagged (stack, node, false);
}

static inline nodeptr_and_flag
interval_stack_pop (struct interval_stack *stack)
{
  if (stack->length == 0)
    return make_nav (NULL, false);
  return stack->nodes[--stack->length];
}


/* +-----------------------------------------------------------------------+ */

/* State used when iterating interval. */
struct itree_iterator
{
  struct interval_stack *stack;
  ptrdiff_t begin;
  ptrdiff_t end;

  /* A copy of the tree's `otick`.  */
  uintmax_t otick;
  enum itree_order order;
  bool running;
  const char *file;
  int line;
};

/* Ideally, every iteration would use its own `iter` object, so we could
   have several iterations active at the same time.  In practice, iterations
   are limited by the fact we don't allow modifying the tree at the same
   time, making the use of nested iterations quite rare anyway.
   So we just use a single global iterator instead for now.  */
static struct itree_iterator *iter = NULL;

static int
interval_tree_max_height (const struct itree_tree *tree)
{
  return 2 * log (tree->size + 1) / log (2) + 0.5;
}

/* Allocate a new iterator for TREE. */

static struct itree_iterator *
itree_iterator_create (struct itree_tree *tree)
{
  struct itree_iterator *g = xmalloc (sizeof *g);
  /* 19 here just avoids starting with a silly-small stack.
     FIXME: Since this stack only needs to be about 2*max_depth
     in the worst case, we could completely pre-allocate it to something
     like word-bit-size * 2 and then never worry about growing it.  */
  const int size = (tree ? interval_tree_max_height (tree) : 19) + 1;

  g->stack = interval_stack_create (size);
  g->running = false;
  g->begin = 0;
  g->end = 0;
  g->file = NULL;
  g->line = 0;
  return g;
}

void
init_itree (void)
{
  eassert (!iter);
  iter = itree_iterator_create (NULL);
}

#ifdef HAVE_UNEXEC
void
forget_itree (void)
{
  iter = NULL;
}
#endif

struct check_subtree_result
{
  /* Node count of the tree.  */
  int size;

  /* Limit of the tree (max END).  */
  ptrdiff_t limit;

  /* Black height of the tree.  */
  int black_height;
};

static struct check_subtree_result
check_subtree (struct itree_node *node,
	       bool check_red_black_invariants, uintmax_t tree_otick,
	       ptrdiff_t offset, ptrdiff_t min_begin,
	       ptrdiff_t max_begin)
{
  struct check_subtree_result result = { .size = 0,
					 .limit = PTRDIFF_MIN,
					 .black_height = 0 };
  if (node == NULL)
    return result;

  /* Validate structure.  */
  eassert (node->left == NULL || node->left->parent == node);
  eassert (node->right == NULL || node->right->parent == node);

  /* Validate otick.  A node's otick must be <= to the tree's otick
     and <= to its parent's otick.

     Note: we cannot assert that (NODE.otick == NODE.parent.otick)
     implies (NODE.offset == 0) because interval_tree_inherit_offset()
     doesn't always update otick.  It could, but it is not clear there
     is a need.  */
  eassert (node->otick <= tree_otick);
  eassert (node->parent == NULL || node->otick <= node->parent->otick);
  eassert (node->otick != tree_otick || node->offset == 0);

  offset += node->offset;
  ptrdiff_t begin = node->begin + offset;
  ptrdiff_t end = node->end + offset;
  ptrdiff_t limit = node->limit + offset;

  eassert (min_begin <= max_begin);
  eassert (min_begin <= begin);
  eassert (begin <= max_begin);
  eassert (end <= limit);

  struct check_subtree_result left_result
    = check_subtree (node->left, check_red_black_invariants,
		     tree_otick, offset, min_begin, begin);
  struct check_subtree_result right_result
    = check_subtree (node->right, check_red_black_invariants,
		     tree_otick, offset, begin, max_begin);

  eassert (left_result.limit <= limit);
  eassert (right_result.limit <= limit);
  eassert (limit == max (end, max (left_result.limit, right_result.limit)));

  if (check_red_black_invariants)
    {
      eassert (left_result.black_height == right_result.black_height);
      eassert (node->parent == NULL || !node->red || !node->parent->red);
    }

  result.size = 1 + left_result.size + right_result.size;
  result.limit = limit;
  result.black_height = (node->red ? 0 : 1) + left_result.black_height;
  return result;
}

/* Validate invariants for TREE.  If CHECK_RED_BLACK_INVARIANTS, red
   nodes with red children are considered invalid.

   This runs in constant time when ENABLE_OVERLAY_CHECKING is 0
   (i.e. Emacs is not configured with
   "--enable_checking=yes,overlays").  In this mode it can't check all
   the invariants.  When ENABLE_OVERLAY_CHECKING is 1 it checks the
   entire tree and validates all invariants.
*/
static bool
check_tree (struct itree_tree *tree,
	    bool check_red_black_invariants)
{
  eassert (tree != NULL);
  eassert (tree->size >= 0);
  eassert ((tree->size == 0) == (tree->root == NULL));
  if (tree->root == NULL)
    return true;
  eassert (tree->root->parent == NULL);
  eassert (!check_red_black_invariants || !tree->root->red);

  struct itree_node *node = tree->root;
  struct check_subtree_result result
    = check_subtree (node, check_red_black_invariants, tree->otick,
		     node->offset, PTRDIFF_MIN,
		     PTRDIFF_MAX);
  eassert (result.size == tree->size);

  /* The only way this function fails is eassert().  */
  return true;
}

/* +=======================================================================+
 * | Internal Functions
 * +=======================================================================+ */

static bool
null_safe_is_red (struct itree_node *node)
{
  return node != NULL && node->red;
}

static bool
null_safe_is_black (struct itree_node *node)
{
  return node == NULL || !node->red; /* NULL nodes are black */
}

static inline ptrdiff_t
itree_newlimit (struct itree_node *node)
{
  eassert (node != NULL);
  return max (node->end,
	      max (node->left == NULL
		     ? PTRDIFF_MIN
		     : node->left->limit + node->left->offset,
		   node->right == NULL
		     ? PTRDIFF_MIN
		     : node->right->limit + node->right->offset));
}

/* Update NODE's limit attribute according to its children. */

static void
interval_tree_update_limit (struct itree_node *node)
{
  if (node == NULL)
    return;

  node->limit = itree_newlimit (node);
}

/* Apply NODE's offset to its begin, end and limit values and
   propagate it to its children.

   Does nothing, if NODE is clean, i.e. NODE.otick = tree.otick .
*/

static void
interval_tree_inherit_offset (uintmax_t otick, struct itree_node *node)
{
  eassert (node->parent == NULL || node->parent->otick >= node->otick);
  if (node->otick == otick)
    {
      eassert (node->offset == 0);
      return;
    }

  /* Offsets can be inherited from dirty nodes (with out of date
     otick) during removal, since we do not travel down from the root
     in that case.  In this case rotations are performed on
     potentially "dirty" nodes, where we only need to make sure the
     *local* offsets are zero.  */

  if (node->offset)
    {
      node->begin += node->offset;
      node->end   += node->offset;
      node->limit += node->offset;
      if (node->left != NULL)
	node->left->offset += node->offset;
      if (node->right != NULL)
	node->right->offset += node->offset;
      node->offset = 0;
    }
  /* The only thing that matters about `otick` is whether it's equal to
     that of the tree.  We could also "blindly" inherit from parent->otick,
     but we need to tree's `otick` anyway for when there's no parent.  */
  if (node->parent == NULL || node->parent->otick == otick)
    node->otick = otick;
}

/* Update limit of NODE and its ancestors.  Stop when it becomes
   stable, i.e. new_limit = old_limit.  */

static void
interval_tree_propagate_limit (struct itree_node *node)
{
  ptrdiff_t newlimit;

  if (node == NULL)
    return;

  while (1)
    {
      newlimit = itree_newlimit (node);

      if (newlimit == node->limit)
	break;
      node->limit = newlimit;
      if (node->parent == NULL)
	break;
      node = node->parent;
    }
}

static struct itree_node*
interval_tree_validate (struct itree_tree *tree, struct itree_node *node)
{

  if (tree->otick == node->otick || node == NULL)
    return node;
  if (node != tree->root)
    interval_tree_validate (tree, node->parent);

  interval_tree_inherit_offset (tree->otick, node);
  return node;
}

/* +=======================================================================+
 * | Tree operations
 * +=======================================================================+ */

/* Initialize an allocated node. */

void
itree_node_init (struct itree_node *node,
		 bool front_advance, bool rear_advance,
		 Lisp_Object data)
{
  node->parent = NULL;
  node->left = NULL;
  node->right = NULL;
  node->begin = -1;
  node->end = -1;
  node->front_advance = front_advance;
  node->rear_advance = rear_advance;
  node->data = data;
}

/* Return NODE's begin value, computing it if necessary. */

ptrdiff_t
itree_node_begin (struct itree_tree *tree,
		  struct itree_node *node)
{
  interval_tree_validate (tree, node);
  return node->begin;
}

/* Return NODE's end value, computing it if necessary. */

ptrdiff_t
itree_node_end (struct itree_tree *tree,
		struct itree_node *node)
{
  interval_tree_validate (tree, node);
  return node->end;
}

/* Allocate an itree_tree.  Free with itree_destroy.  */

struct itree_tree *
itree_create (void)
{
  struct itree_tree *tree = xmalloc (sizeof (*tree));
  itree_clear (tree);
  return tree;
}

/* Reset the tree TREE to its empty state.  */

void
itree_clear (struct itree_tree *tree)
{
  tree->root = NULL;
  tree->otick = 1;
  tree->size = 0;
}

#ifdef ITREE_TESTING
/* Initialize a pre-allocated tree (presumably on the stack).  */

static void
interval_tree_init (struct itree_tree *tree)
{
  itree_clear (tree);
}
#endif

/* Release a tree, freeing its allocated memory.  */
void
itree_destroy (struct itree_tree *tree)
{
  eassert (tree->root == NULL);
  xfree (tree);
}

/* Return the number of nodes in TREE.  */

intmax_t
itree_size (struct itree_tree *tree)
{
  return tree->size;
}

/* Perform the familiar left-rotation on node NODE.  */

static void
interval_tree_rotate_left (struct itree_tree *tree,
			   struct itree_node *node)
{
  eassert (node->right != NULL);

  struct itree_node *right = node->right;

  interval_tree_inherit_offset (tree->otick, node);
  interval_tree_inherit_offset (tree->otick, right);

  /* Turn right's left subtree into node's right subtree.  */
  node->right = right->left;
  if (right->left != NULL)
    right->left->parent = node;

  /* right's parent was node's parent.  */
  if (right != NULL)
    right->parent = node->parent;

  /* Get the parent to point to right instead of node.  */
  if (node != tree->root)
    {
      if (node == node->parent->left)
	node->parent->left = right;
      else
	node->parent->right = right;
    }
  else
    tree->root = right;

  /* Put node on right's left.  */
  right->left = node;
  if (node != NULL)
    node->parent = right;

  /* Order matters here.  */
  interval_tree_update_limit (node);
  interval_tree_update_limit (right);
}

/* Perform the familiar right-rotation on node NODE.  */

static void
interval_tree_rotate_right (struct itree_tree *tree,
			    struct itree_node *node)
{
  eassert (tree && node && node->left != NULL);

  struct itree_node *left = node->left;

  interval_tree_inherit_offset (tree->otick, node);
  interval_tree_inherit_offset (tree->otick, left);

  node->left = left->right;
  if (left->right != NULL)
    left->right->parent = node;

  if (left != NULL)
    left->parent = node->parent;
  if (node != tree->root)
    {
      if (node == node->parent->right)
	node->parent->right = left;
      else
	node->parent->left = left;
    }
  else
    tree->root = left;

  left->right = node;
  if (node != NULL)
    node->parent = left;

  interval_tree_update_limit (left);
  interval_tree_update_limit (node);
}

/* Repair the tree after an insertion.
   The new NODE was added as red, so we may have 2 reds in a row.
   Rebalance the parents as needed to re-establish the RB invariants.  */

static void
interval_tree_insert_fix (struct itree_tree *tree,
			  struct itree_node *node)
{
  eassert (tree->root->red == false);

  while (null_safe_is_red (node->parent))
    {
      /* NODE is red and its parent is red.  This is a violation of
	 red-black tree property #3.  */
      eassert (node->red);

      if (node->parent == node->parent->parent->left)
	{
	  /* We're on the left side of our grandparent, and OTHER is
	     our "uncle".  */
	  struct itree_node *uncle = node->parent->parent->right;

	  if (null_safe_is_red (uncle)) /* case 1.a */
	    {
	      /* Uncle and parent are red but should be black because
		 NODE is red.  Change the colors accordingly and
		 proceed with the grandparent.  */
	      node->parent->red = false;
	      uncle->red = false;
	      node->parent->parent->red = true;
	      node = node->parent->parent;
	    }
	  else
	    {
	      /* Parent and uncle have different colors; parent is
		 red, uncle is black.  */
	      if (node == node->parent->right) /* case 2.a */
		{
		  node = node->parent;
		  interval_tree_rotate_left (tree, node);
		}
	      /* case 3.a */
	      node->parent->red = false;
	      node->parent->parent->red = true;
	      interval_tree_rotate_right (tree, node->parent->parent);
	    }
	}
      else
	{
	  /* This is the symmetrical case of above.  */
	  struct itree_node *uncle = node->parent->parent->left;

	  if (null_safe_is_red (uncle)) /* case 1.b */
	    {
	      node->parent->red = false;
	      uncle->red = false;
	      node->parent->parent->red = true;
	      node = node->parent->parent;
	    }
	  else
	    {
	      if (node == node->parent->left) /* case 2.b */
		{
		  node = node->parent;
		  interval_tree_rotate_right (tree, node);
		}
	      /* case 3.b */
	      node->parent->red = false;
	      node->parent->parent->red = true;
	      interval_tree_rotate_left (tree, node->parent->parent);
	    }
	}
    }

  /* The root may have been changed to red due to the algorithm.
     Set it to black so that property #5 is satisfied.  */
  tree->root->red = false;
  eassert (check_tree (tree, true)); /* FIXME: Too expensive.  */
}

/* Insert a NODE into the TREE.
   Note, that inserting a node twice results in undefined behavior.  */

static void
interval_tree_insert (struct itree_tree *tree, struct itree_node *node)
{
  eassert (node && node->begin <= node->end);
  /* FIXME: The assertion below fails because `delete_all_overlays`
     doesn't set left/right/parent to NULL.  */
  /* eassert (node->left == NULL && node->right == NULL
	     && node->parent == NULL) */;
  eassert (check_tree (tree, true)); /* FIXME: Too expensive.  */

  struct itree_node *parent = NULL;
  struct itree_node *child = tree->root;
  uintmax_t otick = tree->otick;
  /* It's the responsibility of the caller to set `otick` on the node,
     to "confirm" that the begin/end fields are up to date.  */
  eassert (node->otick == otick);

  /* Find the insertion point, accumulate node's offset and update
     ancestors limit values.  */
  while (child != NULL)
    {
      interval_tree_inherit_offset (otick, child);
      parent = child;
      eassert (child->offset == 0);
      child->limit = max (child->limit, node->end);
      /* This suggests that nodes in the right subtree are strictly
	 greater.  But this is not true due to later rotations.  */
      child = node->begin <= child->begin ? child->left : child->right;
    }

  /* Insert the node */
  if (parent == NULL)
    tree->root = node;
  else if (node->begin <= parent->begin)
    parent->left = node;
  else
    parent->right = node;

  /* Init the node */
  node->parent = parent;
  node->left = NULL;
  node->right = NULL;
  node->offset = 0;
  node->limit = node->end;
  eassert (node->parent == NULL || node->parent->otick >= node->otick);

  /* Fix/update the tree */
  ++tree->size;
  if (node == tree->root)
    node->red = false;
  else
    {
      node->red = true;
      eassert (check_tree (tree, false)); /* FIXME: Too expensive.  */
      interval_tree_insert_fix (tree, node);
    }
}

void
itree_insert (struct itree_tree *tree, struct itree_node *node,
	      ptrdiff_t begin, ptrdiff_t end)
{
  node->begin = begin;
  node->end = end;
  node->otick = tree->otick;
  interval_tree_insert (tree, node);
}

/* Safely modify a node's interval. */

void
itree_node_set_region (struct itree_tree *tree,
		       struct itree_node *node,
		       ptrdiff_t begin, ptrdiff_t end)
{
  interval_tree_validate (tree, node);
  if (begin != node->begin)
    {
      itree_remove (tree, node);
      node->begin = min (begin, PTRDIFF_MAX - 1);
      node->end = max (node->begin, end);
      interval_tree_insert (tree, node);
    }
  else if (end != node->end)
    {
      node->end = max (node->begin, end);
      eassert (node != NULL);
      interval_tree_propagate_limit (node);
    }
}

/* Return true, if NODE is a member of TREE. */

static bool
interval_tree_contains (struct itree_tree *tree, struct itree_node *node)
{
  eassert (iter && node);
  struct itree_node *other;
  ITREE_FOREACH (other, tree, node->begin, PTRDIFF_MAX, ASCENDING)
    if (other == node)
      {
	ITREE_FOREACH_ABORT ();
	return true;
      }

  return false;
}

static bool
itree_limit_is_stable (struct itree_node *node)
{
  if (node == NULL)
    return true;
  ptrdiff_t newlimit = itree_newlimit (node);
  return (newlimit == node->limit);
}

static struct itree_node*
interval_tree_subtree_min (uintmax_t otick, struct itree_node *node)
{
  if (node == NULL)
    return node;
  while ((interval_tree_inherit_offset (otick, node),
	  node->left != NULL))
    node = node->left;
  return node;
}

/* Repair the tree after a deletion.
   The black-depth of NODE is one less than that of its sibling,
   so re-balance the parents to re-establish the RB invariants.  */

static void
interval_tree_remove_fix (struct itree_tree *tree,
			  struct itree_node *node,
			  struct itree_node *parent)
{
  if (parent == NULL)
    eassert (node == tree->root);
  else
    eassert (node == NULL || node->parent == parent);

  while (parent != NULL && null_safe_is_black (node))
    {
      eassert (node == parent->left || node == parent->right);

      if (node == parent->left)
	{
	  struct itree_node *other = parent->right;

	  if (null_safe_is_red (other)) /* case 1.a */
	    {
	      other->red = false;
	      parent->red = true;
	      interval_tree_rotate_left (tree, parent);
	      other = parent->right;
	    }
	  eassume (other != NULL);

	  if (null_safe_is_black (other->left) /* 2.a */
	      && null_safe_is_black (other->right))
	    {
	      other->red = true;
	      node = parent;
	      eassert (node != NULL);
	      parent = node->parent;
	    }
	  else
	    {
	      if (null_safe_is_black (other->right)) /* 3.a */
		{
		  other->left->red = false;
		  other->red = true;
		  interval_tree_rotate_right (tree, other);
		  other = parent->right;
		}
	      other->red = parent->red; /* 4.a */
	      parent->red = false;
	      other->right->red = false;
	      interval_tree_rotate_left (tree, parent);
	      node = tree->root;
	      parent = NULL;
	    }
	}
      else
	{
	  struct itree_node *other = parent->left;

	  if (null_safe_is_red (other)) /* 1.b */
	    {
	      other->red = false;
	      parent->red = true;
	      interval_tree_rotate_right (tree, parent);
	      other = parent->left;
	    }
	  eassume (other != NULL);

	  if (null_safe_is_black (other->right) /* 2.b */
	      && null_safe_is_black (other->left))
	    {
	      other->red = true;
	      node = parent;
	      eassert (node != NULL);
	      parent = node->parent;
	    }
	  else
	    {
	      if (null_safe_is_black (other->left)) /* 3.b */
		{
		  other->right->red = false;
		  other->red = true;
		  interval_tree_rotate_left (tree, other);
		  other = parent->left;
		}

	      other->red = parent->red; /* 4.b */
	      parent->red = false;
	      other->left->red = false;
	      interval_tree_rotate_right (tree, parent);
	      node = tree->root;
	      parent = NULL;
	    }
	}
    }

  if (node != NULL)
    node->red = false;
}

/* Return accumulated offsets of NODE's parents.  */
static ptrdiff_t
itree_total_offset (struct itree_node *node)
{
  eassert (node != NULL);
  ptrdiff_t offset = 0;
  while (node->parent != NULL)
    {
      node = node->parent;
      offset += node->offset;
    }
  return offset;
}

/* Replace DEST with SOURCE as a child of DEST's parent.  Adjusts
   *only* the parent linkage of SOURCE and either the parent's child
   link the tree root.

   Warning: DEST is left unmodified.  SOURCE's child links are
   unchanged.  Caller is responsible for recalculation of `limit`.
   Requires both nodes to be using the same effective `offset`.  */
static void
interval_tree_replace_child (struct itree_tree *tree,
			     struct itree_node *source,
			     struct itree_node *dest)
{
  eassert (tree && dest != NULL);
  eassert (source == NULL
	   || itree_total_offset (source) == itree_total_offset (dest));

  if (dest == tree->root)
    tree->root = source;
  else if (dest == dest->parent->left)
    dest->parent->left = source;
  else
    dest->parent->right = source;

  if (source != NULL)
    source->parent = dest->parent;
}
/* Replace DEST with SOURCE in the tree.  Copies the following fields
   from DEST to SOURCE: red, parent, left, right.  Also updates
   parent, left and right in surrounding nodes to point to SOURCE.

   Warning: DEST is left unmodified.  Caller is responsible for
   recalculation of `limit`.  Requires both nodes to be using the same
   effective `offset`. */
static void
interval_tree_transplant (struct itree_tree *tree,
			  struct itree_node *source,
			  struct itree_node *dest)
{
  interval_tree_replace_child (tree, source, dest);
  source->left = dest->left;
  if (source->left != NULL)
    source->left->parent = source;
  source->right = dest->right;
  if (source->right != NULL)
    source->right->parent = source;
  source->red = dest->red;
}

/* Remove NODE from TREE and return it.  NODE must exist in TREE.  */

struct itree_node*
itree_remove (struct itree_tree *tree, struct itree_node *node)
{
  eassert (interval_tree_contains (tree, node));
  eassert (check_tree (tree, true)); /* FIXME: Too expensive.  */

  /* Find `splice`, the leaf node to splice out of the tree.  When
     `node` has at most one child this is `node` itself.  Otherwise,
     it is the in order successor of `node`.  */
  interval_tree_inherit_offset (tree->otick, node);
  struct itree_node *splice
    = (node->left == NULL || node->right == NULL)
	? node
	: interval_tree_subtree_min (tree->otick, node->right);

  /* Find `subtree`, the only child of `splice` (may be NULL).  Note:
     `subtree` will not be modified other than changing its parent to
     `splice`.  */
  eassert (splice->left == NULL || splice->right == NULL);
  struct itree_node *subtree
    = (splice->left != NULL) ? splice->left : splice->right;

  /* Save a pointer to the parent of where `subtree` will eventually
     be in `subtree_parent`.  */
  struct itree_node *subtree_parent
    = (splice->parent != node) ? splice->parent : splice;

  /* If `splice` is black removing it may violate Red-Black
     invariants, so note this for later.  */

  /* Replace `splice` with `subtree` under subtree's parent.  If
     `splice` is black, this creates a red-red violation, so remember
     this now as the field can be overwritten when splice is
     transplanted below.  */
  interval_tree_replace_child (tree, subtree, splice);
  bool removed_black = !splice->red;

  /* Replace `node` with `splice` in the tree and propagate limit
     upwards, if necessary.  Note: Limit propagation can stabilize at
     any point, so we must call from bottom to top for every node that
     has a new child.  */
  if (splice != node)
    {
      interval_tree_transplant (tree, splice, node);
      interval_tree_propagate_limit (subtree_parent);
      if (splice != subtree_parent)
	interval_tree_update_limit (splice);
    }
  interval_tree_propagate_limit (splice->parent);

  --tree->size;

  /* Fix any black height violation caused by removing a black node.  */
  if (removed_black)
    interval_tree_remove_fix (tree, subtree, subtree_parent);

  eassert ((tree->size == 0) == (tree->root == NULL));
  eassert (check_tree (tree, true)); /* FIXME: Too expensive.  */

  /* Clear fields related to the tree for sanity while debugging.  */
  node->red = false;
  node->right = node->left = node->parent = NULL;
  node->limit = 0;

  /* Must be clean (all offsets applied).  Also, some callers rely on
     node's otick being the tree's otick.  */
  eassert (node->otick == tree->otick);
  eassert (node->offset == 0);

  return node;
}

bool
itree_iterator_busy_p (void)
{
  return (iter && iter->running);
}

/* Start a iterator enumerating all intervals in [BEGIN,END) in the
   given ORDER.  Only one iterator per tree can be running at any time.  */

struct itree_iterator *
itree_iterator_start (struct itree_tree *tree, ptrdiff_t begin,
		      ptrdiff_t end, enum itree_order order,
		      const char *file, int line)
{
  eassert (iter);
  if (iter->running)
    {
      fprintf (stderr,
	       "Detected nested iteration!\nOuter: %s:%d\nInner: %s:%d\n",
	       iter->file, iter->line, file, line);
      emacs_abort ();
    }
  iter->begin = begin;
  iter->end = end;
  iter->otick = tree->otick;
  iter->order = order;
  interval_stack_clear (iter->stack);
  if (begin <= end && tree->root != NULL)
    interval_stack_push_flagged (iter->stack, tree->root, false);
  iter->file = file;
  iter->line = line;
  iter->running = true;
  /* interval_stack_ensure_space (iter->stack,
				  2 * interval_tree_max_height (tree)); */
  return iter;
}

/* Stop using the iterator. */

void
itree_iterator_finish (struct itree_iterator *iter)
{
  eassert (iter && iter->running);
  iter->running = false;
}


/* +=======================================================================+
 * | Insert/Delete Gaps
 * +=======================================================================+ */

/* Insert a gap at POS of length LENGTH expanding all intervals
   intersecting it, while respecting their rear_advance and
   front_advance setting.

   If BEFORE_MARKERS is non-zero, all overlays beginning/ending at POS
   are treated as if their front_advance/rear_advance was true. */

void
itree_insert_gap (struct itree_tree *tree,
		  ptrdiff_t pos, ptrdiff_t length, bool before_markers)
{
  if (!tree || length <= 0 || tree->root == NULL)
    return;
  uintmax_t ootick = tree->otick;

  /* FIXME: Don't allocate iterator/stack anew every time. */

  /* Nodes with front_advance starting at pos may mess up the tree
     order, so we need to remove them first.  This doesn't apply for
     `before_markers` since in that case, all positions move identically
     regardless of `front_advance` or `rear_advance`.  */
  struct interval_stack *saved = interval_stack_create (0);
  struct itree_node *node = NULL;
  if (!before_markers)
    {
      ITREE_FOREACH (node, tree, pos, pos + 1, PRE_ORDER)
	{
	  if (node->begin == pos && node->front_advance
	      /* If we have front_advance and !rear_advance and
	         the overlay is empty, make sure we don't move
	         begin past end by pretending it's !front_advance.  */
	      && (node->begin != node->end || node->rear_advance))
	    interval_stack_push (saved, node);
	}
    }
  for (size_t i = 0; i < saved->length; ++i)
    itree_remove (tree, nav_nodeptr (saved->nodes[i]));

  /* We can't use an iterator here, because we can't effectively
     narrow AND shift some subtree at the same time.  */
  if (tree->root != NULL)
    {
      const int size = interval_tree_max_height (tree) + 1;
      struct interval_stack *stack = interval_stack_create (size);
      interval_stack_push (stack, tree->root);
      nodeptr_and_flag nav;
      while ((nav = interval_stack_pop (stack),
	      node = nav_nodeptr (nav)))
	{
	  /* Process in pre-order. */
	  interval_tree_inherit_offset (tree->otick, node);
	  if (pos > node->limit)
	    continue;
	  if (node->right != NULL)
	    {
	      if (node->begin > pos)
		{
		  /* All nodes in this subtree are shifted by length.  */
		  node->right->offset += length;
		  ++tree->otick;
		}
	      else
		interval_stack_push (stack, node->right);
	    }
	  if (node->left != NULL)
	    interval_stack_push (stack, node->left);

	  if (before_markers
	      ? node->begin >= pos
	      : node->begin > pos) /* node->begin == pos => !front-advance  */
	    node->begin += length;
	  if (node->end > pos
	      || (node->end == pos && (before_markers || node->rear_advance)))
	    {
	      node->end += length;
	      eassert (node != NULL);
	      interval_tree_propagate_limit (node);
	    }
	}
      interval_stack_destroy (stack);
    }

  /* Reinsert nodes starting at POS having front-advance.  */
  uintmax_t notick = tree->otick;
  nodeptr_and_flag nav;
  while ((nav = interval_stack_pop (saved),
	  node = nav_nodeptr (nav)))
    {
      eassert (node->otick == ootick);
      eassert (node->begin == pos);
      eassert (node->end > pos || node->rear_advance);
      node->begin += length;
      node->end += length;
      node->otick = notick;
      interval_tree_insert (tree, node);
    }

  interval_stack_destroy (saved);
}

/* Delete a gap at POS of length LENGTH, contracting all intervals
   intersecting it.  */

void
itree_delete_gap (struct itree_tree *tree,
		  ptrdiff_t pos, ptrdiff_t length)
{
  if (!tree || length <= 0 || tree->root == NULL)
    return;

  /* FIXME: Don't allocate stack anew every time.  */

  /* Can't use the iterator here, because by decrementing begin, we
     might unintentionally bring shifted nodes back into our search space.  */
  const int size = interval_tree_max_height (tree) + 1;
  struct interval_stack *stack = interval_stack_create (size);
  struct itree_node *node;

  interval_stack_push (stack, tree->root);
  nodeptr_and_flag nav;
  while ((nav = interval_stack_pop (stack)))
    {
      node = nav_nodeptr (nav);
      interval_tree_inherit_offset (tree->otick, node);
      if (pos > node->limit)
	continue;
      if (node->right != NULL)
	{
	  if (node->begin > pos + length)
	    {
	      /* Shift right subtree to the left. */
	      node->right->offset -= length;
	      ++tree->otick;
	    }
	  else
	    interval_stack_push (stack, node->right);
	}
      if (node->left != NULL)
	interval_stack_push (stack, node->left);

      if (pos < node->begin)
	node->begin = max (pos, node->begin - length);
      if (node->end > pos)
	{
	  node->end = max (pos , node->end - length);
	  eassert (node != NULL);
	  interval_tree_propagate_limit (node);
	}
    }
  interval_stack_destroy (stack);
}



/* +=======================================================================+
 * | Iterator
 * +=======================================================================+ */

/* Return true, if NODE's interval intersects with [BEGIN, END).
   Note: We always include empty nodes at BEGIN (and not at END),
   but if BEGIN==END, then we don't include non-empty nodes starting
   at BEGIN or ending at END.  This seems to match the behavior of the
   old overlays code but it's not clear if it's The Right Thing
   (e.g. it breaks the expectation that if NODE1 is included, then
   a NODE2 strictly bigger than NODE1 should also be included).  */

static inline bool
interval_node_intersects (const struct itree_node *node,
			  ptrdiff_t begin, ptrdiff_t end)
{
  return (begin < node->end && node->begin < end)
    || (node->begin == node->end && begin == node->begin);
}

/* Return the next node of the iterator in the order given when it was
   started; or NULL if there are no more nodes. */

struct itree_node *
itree_iterator_next (struct itree_iterator *g)
{
  eassert (g && g->running);

  struct itree_node *const null = NULL;
  struct itree_node *node;

  /* The `visited` flag stored in each node is used here (and only here):
     We keep a "workstack" of nodes we need to consider.  This stack
     consist of nodes of two types: nodes that we have decided
     should be returned by the iterator, and nodes which we may
     need to consider (including checking their children).
     We start an iteration with a stack containing just the root
     node marked as "not visited" which means that it (and its children)
     needs to be considered but we haven't yet decided whether it's included
     in the iterator's output.  */

  do
    {
      nodeptr_and_flag nav;
      bool visited;
      while ((nav = interval_stack_pop (g->stack),
	      node = nav_nodeptr (nav),
	      visited = nav_flag (nav),
	      node && !visited))
	{
	  struct itree_node *const left = node->left;
	  struct itree_node *const right = node->right;

	  interval_tree_inherit_offset (g->otick, node);
	  eassert (itree_limit_is_stable (node));
	  switch (g->order)
	    {
	    case ITREE_ASCENDING:
	      if (right != null && node->begin <= g->end)
		interval_stack_push_flagged (g->stack, right, false);
	      if (interval_node_intersects (node, g->begin, g->end))
		interval_stack_push_flagged (g->stack, node, true);
	      /* Node's children may still be off-set and we need to add it.  */
	      if (left != null && g->begin <= left->limit + left->offset)
		interval_stack_push_flagged (g->stack, left, false);
	      break;
	    case ITREE_DESCENDING:
	      if (left != null && g->begin <= left->limit + left->offset)
		interval_stack_push_flagged (g->stack, left, false);
	      if (interval_node_intersects (node, g->begin, g->end))
		interval_stack_push_flagged (g->stack, node, true);
	      if (right != null && node->begin <= g->end)
		interval_stack_push_flagged (g->stack, right, false);
	      break;
	    case ITREE_PRE_ORDER:
	      if (right != null && node->begin <= g->end)
		interval_stack_push_flagged (g->stack, right, false);
	      if (left != null && g->begin <= left->limit + left->offset)
		interval_stack_push_flagged (g->stack, left, false);
	      if (interval_node_intersects (node, g->begin, g->end))
		interval_stack_push_flagged (g->stack, node, true);
	      break;
	    }
	}
      /* Node may have been invalidated by itree_iterator_narrow
	 after it was pushed: Check if it still intersects. */
    } while (node && ! interval_node_intersects (node, g->begin, g->end));

  return node;
}

/* Limit G to the new interval [BEGIN, END), which must be a subset of
   the current one.  I.E. it can't grow on either side. */

void
itree_iterator_narrow (struct itree_iterator *g,
		       ptrdiff_t begin, ptrdiff_t end)
{
  eassert (g && g->running);
  eassert (begin >= g->begin);
  eassert (end <= g->end);
  g->begin = max (begin, g->begin);
  g->end = min (end, g->end);
}
