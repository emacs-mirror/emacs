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
   space over END postions.

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
   incremented whenever some node's offset has changed.
*/

/* FIXME: The code seems to use "generator" and "iterator"
   inconsistently/interchangeably.  We should fix this naming.  */

static struct interval_node *interval_tree_validate (struct interval_tree *, struct interval_node *);
static bool interval_node_intersects (const struct interval_node *, ptrdiff_t, ptrdiff_t);
static int interval_tree_max_height (const struct interval_tree *);
static void interval_tree_update_limit (struct interval_node *);
static void interval_tree_inherit_offset (uintmax_t otick, struct interval_node *);
static void interval_tree_propagate_limit (struct interval_node *);
static void interval_tree_rotate_left (struct interval_tree *, struct interval_node *);
static void interval_tree_rotate_right (struct interval_tree *, struct interval_node *);
static void interval_tree_insert_fix (struct interval_tree *, struct interval_node *);
static void interval_tree_transplant (struct interval_tree *, struct interval_node *, struct interval_node *);
static struct interval_generator* interval_generator_create (struct interval_tree *);

/* The sentinel node, the null node.  */
struct interval_node itree_null;

static bool
null_is_sane (void)
{
  /* The sentinel node has most of its fields read-only, except for `parent`,
     `left`, `right` which are write only.  */
  return itree_null.red == false
         && itree_null.otick == 0
         && itree_null.offset == 0
         && itree_null.begin == PTRDIFF_MIN
         && itree_null.end == PTRDIFF_MIN
         && itree_null.limit == PTRDIFF_MIN;
}

/* +------------------------------------------------------------------------------------+ */

typedef uintptr_t nodeptr_and_flag;

/* Simple dynamic array. */
struct interval_stack
{
  nodeptr_and_flag *nodes;
  size_t size;
  size_t length;
};

/* State used when iterating interval. */
struct interval_generator
{
  struct interval_stack *stack;
  ptrdiff_t begin;
  ptrdiff_t end;
  uintmax_t otick;              /* A copy of the tree's `otick`.  */
  enum interval_tree_order order;
  bool running;
  const char* file;
  int line;
};

/* Ideally, every iteration would use its own `iter` object, so we could
   have several iterations active at the same time.  In practice, iterations
   are limited by the fact we don't allow modifying the tree at the same
   time, making the use of nested iterations quite rare anyway.
   So we just use a single global iterator instead for now.  */
static struct interval_generator *iter;

static void
itree_init (void)
{
  struct interval_node *null = ITREE_NULL;
  null->left = null->right = null->parent = null;
  null->offset = null->otick = 0;
  null->begin = PTRDIFF_MIN;
  null->end = PTRDIFF_MIN;
  null->limit = PTRDIFF_MIN;     /* => max(x, null.limit) = x */
  null->red = false;
  iter = interval_generator_create (NULL);
}

static ptrdiff_t
recurse_check_tree (struct interval_node *node, uintmax_t tree_otick,
                    ptrdiff_t offset, ptrdiff_t min_begin,
                    ptrdiff_t max_begin, intmax_t *size)
{
  if (node == ITREE_NULL)
    return PTRDIFF_MIN;
  ++*size;

  /* Validate structure.  */
  eassert (
    node->parent == ITREE_NULL
    || (node->parent->left == node || node->parent->right == node));
  eassert (node->left == ITREE_NULL || node->left->parent == node);
  eassert (node->right == ITREE_NULL || node->right->parent == node);

  /* Red nodes cannot have red parents.  */
  eassert (node->parent == ITREE_NULL
           || !(node->red && node->parent->red));

  eassert (node->offset == 0 || node->otick < tree_otick);

  offset += node->offset;
  ptrdiff_t begin = node->begin + offset;
  ptrdiff_t end = node->end + offset;
  ptrdiff_t limit = node->limit + offset;

  eassert (min_begin <= max_begin);
  eassert (min_begin <= begin);
  eassert (begin <= max_begin);
  eassert (end <= limit);

  ptrdiff_t left_limit
    = recurse_check_tree (node->left, tree_otick, offset, min_begin,
                          begin, size);
  ptrdiff_t right_limit
    = recurse_check_tree (node->right, tree_otick, offset, begin,
                          max_begin, size);
  eassert (left_limit <= limit);
  eassert (right_limit <= limit);
  eassert (limit == max (end, max (left_limit, right_limit)));
  return limit;
}

static bool
check_tree (struct interval_tree *tree)
{
  eassert (tree != NULL);
  eassert ((tree->size == 0) == (tree->root == ITREE_NULL));
  if (tree->root == ITREE_NULL)
    return true;

  intmax_t size = 0;
  ptrdiff_t offset = tree->root->offset;
  ptrdiff_t limit
    = recurse_check_tree (tree->root, tree->otick, offset,
                          PTRDIFF_MIN, PTRDIFF_MAX, &size);
  eassert (limit == tree->root->limit + offset);
  return true;
}

/* +===================================================================================+
 * | Stack
 * +===================================================================================+ */

static inline nodeptr_and_flag
make_nav (struct interval_node *ptr, bool flag)
{
  uintptr_t v = (uintptr_t) ptr;
  /* We assume alignment imposes the LSB is clear for us to use it.  */
  eassert (!(v & 1));
  return v | !!flag;
}

static inline struct interval_node *
nav_nodeptr (nodeptr_and_flag nav)
{
  return (struct interval_node *) (nav & (~(uintptr_t)1));
}

static inline bool
nav_flag (nodeptr_and_flag nav)
{
  return (bool) (nav & 1);
}

/* This is just a simple dynamic array with stack semantics. */

static struct interval_stack*
interval_stack_create (intmax_t initial_size)
{
  struct interval_stack *stack = xmalloc (sizeof (struct interval_stack));
  stack->size = max (0, initial_size);
  stack->nodes = xmalloc (stack->size * sizeof (struct interval_node*));
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
interval_stack_ensure_space (struct interval_stack *stack, intmax_t nelements)
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
                             struct interval_node *node, bool flag)
{
  eassert (node && node != ITREE_NULL);

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
interval_stack_push (struct interval_stack *stack, struct interval_node *node)
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


/* +===================================================================================+
 * | Tree operations
 * +===================================================================================+ */

/* Initialize an allocated node. */

void
interval_node_init (struct interval_node *node,
                    ptrdiff_t begin, ptrdiff_t end,
                    bool front_advance, bool rear_advance,
                    Lisp_Object data)
{
  node->begin = begin;
  node->end = max (begin, end);
  node->front_advance = front_advance;
  node->rear_advance = rear_advance;
  node->data = data;
}

/* Return NODE's begin value, computing it if necessary. */

ptrdiff_t
interval_node_begin (struct interval_tree *tree,
                     struct interval_node *node)
{
  interval_tree_validate (tree, node);
  return node->begin;
}

/* Return NODE's end value, computing it if necessary. */

ptrdiff_t
interval_node_end (struct interval_tree *tree,
                   struct interval_node *node)
{
  interval_tree_validate (tree, node);
  return node->end;
}

/* Safely modify a node's interval. */

void
interval_node_set_region (struct interval_tree *tree,
                          struct interval_node *node,
                          ptrdiff_t begin, ptrdiff_t end)
{
  interval_tree_validate (tree, node);
  if (begin != node->begin)
    {
      interval_tree_remove (tree, node);
      node->begin = min (begin, PTRDIFF_MAX - 1);
      node->end = max (node->begin, end);
      interval_tree_insert (tree, node);
    }
  else if (end != node->end)
    {
      node->end = max (node->begin, end);
      eassert (node != ITREE_NULL);
      interval_tree_propagate_limit (node);
    }
}

/* Allocate an interval_tree. Free with interval_tree_destroy. */

struct interval_tree*
interval_tree_create (void)
{
  /* FIXME?  Maybe avoid the initialization of itree_null in the same
     way that is used to call mem_init in alloc.c?  It's not really
     important though.  */
  itree_init ();

  struct interval_tree *tree = xmalloc (sizeof (*tree));
  interval_tree_clear (tree);
  return tree;
}

/* Reset the tree TREE to its empty state.  */

void
interval_tree_clear (struct interval_tree *tree)
{
  tree->root = ITREE_NULL;
  tree->otick = 1;
  tree->size = 0;
}

#ifdef ITREE_TESTING
/* Initialize a pre-allocated tree (presumably on the stack).  */

static void
interval_tree_init (struct interval_tree *tree)
{
  interval_tree_clear (tree);
  /* tree->iter = interval_generator_create (tree); */
}
#endif

/* Release a tree, freeing its allocated memory.  */
void
interval_tree_destroy (struct interval_tree *tree)
{
  eassert (tree->root == ITREE_NULL);
  /* if (tree->iter)
   *   interval_generator_destroy (tree->iter); */
  xfree (tree);
}

/* Return the number of nodes in TREE.  */

intmax_t
interval_tree_size (struct interval_tree *tree)
{
  return tree->size;
}

/* Insert a NODE into the TREE.

   Note, that inserting a node twice results in undefined behaviour.
*/

void
interval_tree_insert (struct interval_tree *tree, struct interval_node *node)
{
  eassert (node && node->begin <= node->end && node != ITREE_NULL);
  eassert (check_tree (tree));

  struct interval_node *parent = ITREE_NULL;
  struct interval_node *child = tree->root;
  ptrdiff_t offset = 0;

  /* Find the insertion point, accumulate node's offset and update
     ancestors limit values.  */
  while (child != ITREE_NULL)
    {
      parent = child;
      offset += child->offset;
      child->limit = max (child->limit, node->end - offset);
      /* This suggests that nodes in the right subtree are strictly
         greater.  But this is not true due to later rotations.  */
      child = node->begin <= child->begin ? child->left : child->right;
    }

  /* Insert the node */
  if (parent == ITREE_NULL)
    tree->root = node;
  else if (node->begin <= parent->begin)
    parent->left = node;
  else
    parent->right = node;

  /* Init the node */
  node->parent = parent;
  node->left = ITREE_NULL;
  node->right = ITREE_NULL;
  node->red = true;
  node->offset = 0;
  node->begin -= offset;
  node->end -= offset;
  node->limit = node->end;
  node->otick = tree->otick - 1;

  /* Fix/update the tree */
  ++tree->size;
  interval_tree_insert_fix (tree, node);
  eassert (check_tree (tree));
}

/* Return true, if NODE is a member of TREE. */

static bool
interval_tree_contains (struct interval_tree *tree, struct interval_node *node)
{
  eassert (node);
  struct interval_node *other;
  ITREE_FOREACH (other, tree, node->begin, PTRDIFF_MAX, ASCENDING)
    if (other == node)
      {
        ITREE_FOREACH_ABORT ();
        return true;
      }

  return false;
}

static inline ptrdiff_t
itree_newlimit (struct interval_node *node)
{
  eassert (node != ITREE_NULL);
  return max (node->end,
              max (node->left->limit + node->left->offset,
                   node->right->limit + node->right->offset));
}

static bool
itree_limit_is_stable (struct interval_node *node)
{
  if (node == ITREE_NULL)
    return true;
  ptrdiff_t newlimit = itree_newlimit (node);
  return (newlimit == node->limit);
}

static inline bool
itree_limits_are_stable (struct interval_node *node)
{
  if (node == ITREE_NULL)
    return true;
  return itree_limit_is_stable (node)
         && itree_limits_are_stable (node->right)
         && itree_limits_are_stable (node->left);
}

static struct interval_node*
interval_tree_subtree_min (uintmax_t otick, struct interval_node *node)
{
  if (node == ITREE_NULL)
    return node;
  while ((interval_tree_inherit_offset (otick, node),
          node->left != ITREE_NULL))
    node = node->left;
  return node;
}

/* Repair the tree after a deletion.
   The black-depth of NODE is one less than that of its sibling,
   so re-balance the parents to re-establish the RB invariants.  */

static void
interval_tree_remove_fix (struct interval_tree *tree,
                          struct interval_node *node,
                          struct interval_node *parent)
{
  eassert (node == ITREE_NULL || node->parent == parent);
  eassert (parent == ITREE_NULL
           || node == parent->left || node == parent->right);

  while (parent != ITREE_NULL && !node->red)
    {
      if (node == parent->left)
	{
	  struct interval_node *other = parent->right;

	  if (other->red) /* case 1.a */
	    {
	      other->red = false;
	      parent->red = true;
	      interval_tree_rotate_left (tree, parent);
	      parent = node->parent;
	      other = parent->right;
            }

	  if (!other->left->red /* 2.a */
              && !other->right->red)
	    {
	      other->red = true;
	      node = parent;
	      eassert (node != ITREE_NULL);
	      parent = node->parent;
            }
	  else
	    {
	      if (!other->right->red) /* 3.a */
		{
		  other->left->red = false;
		  other->red = true;
		  interval_tree_rotate_right (tree, other);
		  parent = node->parent;
		  other = parent->right;
                }
	      other->red = parent->red; /* 4.a */
	      parent->red = false;
	      other->right->red = false;
	      interval_tree_rotate_left (tree, parent);
	      node = tree->root;
	      parent = ITREE_NULL;
            }
        }
      else
	{
	  struct interval_node *other = parent->left;

	  if (other->red) /* 1.b */
	    {
	      other->red = false;
	      parent->red = true;
	      interval_tree_rotate_right (tree, parent);
	      parent = node->parent;
	      other = parent->left;
            }

	  if (!other->right->red /* 2.b */
              && !other->left->red)
	    {
	      other->red = true;
	      node = parent;
	      eassert (node != ITREE_NULL);
	      parent = node->parent;
            }
	  else
	    {
	      if (!other->left->red) /* 3.b */
		{
		  other->right->red = false;
		  other->red = true;
		  interval_tree_rotate_left (tree, other);
		  parent = node->parent;
		  other = parent->left;
                }

	      other->red = parent->red; /* 4.b */
	      parent->red = false;
	      other->left->red = false;
	      interval_tree_rotate_right (tree, parent);
	      node = tree->root;
	      parent = ITREE_NULL;
            }
        }
    }

  node->red = false;
}

/* Remove NODE from TREE and return it.  NODE must exist in TREE.  */

struct interval_node*
interval_tree_remove (struct interval_tree *tree, struct interval_node *node)
{
  /* eassert (itree_limits_are_stable (tree->root)); */
  eassert (interval_tree_contains (tree, node));
  eassert (check_tree (tree));

  /* `broken`, if non-NULL, holds a node that's being moved up to where a black
     node used to be, which may thus require further fixups in its parents
     (done in `interval_tree_remove_fix`).  */
  struct interval_node *broken = NULL;
  /* `broken` may be null but `interval_tree_remove_fix` still
     needs to know its "parent".
     Cormen et al.'s Introduction to Algorithms uses a trick where
     they rely on the null sentinel node's `parent` field to hold
     the right value.  While this works, it breaks the rule that
     the `parent` field is write-only making correctness much more tricky
     and introducing a dependency on a global state (which is incompatible
     with concurrency among other things), so instead we keep track of
     `broken`'s parent manually.  */
  struct interval_node *broken_parent = NULL;

  interval_tree_inherit_offset (tree->otick, node);
  if (node->left == ITREE_NULL || node->right == ITREE_NULL)
    {
      struct interval_node *subst
	= node->right == ITREE_NULL ? node->left : node->right;
      if (!node->red)
        {
          broken = subst;
          broken_parent = node->parent; /* The future parent.  */
        }
      interval_tree_transplant (tree, subst, node);
    }
  else
    {
      struct interval_node *min
        = interval_tree_subtree_min (tree->otick, node->right);
      struct interval_node *min_right = min->right;
      struct interval_node *min_parent = min->parent;

      if (!min->red)
        broken = min_right;
      eassert (min != ITREE_NULL);
      /* `min` should not have any offsets any more so we can move nodes
         underneath it without risking changing their begin/end.  */
      eassert (min->offset == 0);
      if (min->parent == node)
        broken_parent = min; /* The future parent.  */
      else
        {
          interval_tree_transplant (tree, min_right, min);
          broken_parent = min->parent; /* The parent.  */
          min->right = node->right;
        }
      min->left = node->left;
      min->left->parent = min;
      min->red = node->red;
      /* FIXME: At this point node->right->parent = min but node->right
         is a parent of `min` so total_offsets gets stuck in an inf-loop!  */
      interval_tree_transplant (tree, min, node);
      /* We set min->right->parent after `interval_tree_transplant` so
         that calls to `itree_total_offset` don't get stuck in an inf-loop.  */
      min->right->parent = min;
      interval_tree_update_limit (min);
      /* This call "belongs" with the first `interval_tree_transplant`
         (of `min_right`, done earlier in the `if`) but we prefer to do it
         here ("late") because otherwise it would sometimes update part of
         the tree with values that would be invalidated by the second
         `interval_tree_transplant`.  */
      interval_tree_propagate_limit (min_parent);
    }
  interval_tree_propagate_limit (node->parent);

  if (broken)
    interval_tree_remove_fix (tree, broken, broken_parent);

  node->right = node->left = node->parent = NULL;
  --tree->size;

  eassert ((tree->size == 0) == (tree->root == ITREE_NULL));
  /* eassert (itree_limits_are_stable (tree->root)); */
  eassert (check_tree (tree));

  return node;
}

static struct interval_node*
interval_tree_validate (struct interval_tree *tree, struct interval_node *node)
{

  if (tree->otick == node->otick || node == ITREE_NULL)
    return node;
  if (node != tree->root)
    interval_tree_validate (tree, node->parent);

  interval_tree_inherit_offset (tree->otick, node);
  return node;
}

bool
itree_busy_p (void)
{
  return (iter && iter->running);
}

/* Start a generator iterating all intervals in [BEGIN,END) in the
   given ORDER. Only one iterator per tree can be running at any
   time.
*/

struct interval_generator *
interval_tree_iter_start (struct interval_tree *tree,
                          ptrdiff_t begin, ptrdiff_t end,
                          enum interval_tree_order order,
			  const char* file, int line)
{
  eassert (null_is_sane ());
  /* struct interval_generator *iter = tree->iter; */
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
  if (begin <= end && tree->root != ITREE_NULL)
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
interval_tree_iter_finish (struct interval_generator *iter)
{
  eassert (iter->running);
  iter->running = false;
}

static int
interval_tree_max_height (const struct interval_tree *tree)
{
  return 2 * log (tree->size + 1) / log (2) + 0.5;
}


/* +===================================================================================+
 * | Insert/Delete Gaps
 * +===================================================================================+ */

/* Insert a gap at POS of length LENGTH expanding all intervals
   intersecting it, while respecting their rear_advance and
   front_advance setting. */

void
interval_tree_insert_gap (struct interval_tree *tree, ptrdiff_t pos, ptrdiff_t length)
{
  if (length <= 0 || tree->root == ITREE_NULL)
    return;

  /* FIXME: Don't allocate generator/stack anew every time. */

  /* Nodes with front_advance starting at pos may mess up the tree
     order, so we need to remove them first. */
  struct interval_stack *saved = interval_stack_create (0);
  struct interval_node *node = NULL;
  ITREE_FOREACH (node, tree, pos, pos + 1, PRE_ORDER)
    {
      if (node->begin == pos && node->front_advance
          && (node->begin != node->end || node->rear_advance))
        interval_stack_push (saved, node);
    }
  for (int i = 0; i < saved->length; ++i)
    interval_tree_remove (tree, nav_nodeptr (saved->nodes[i]));

  /* We can't use a generator here, because we can't effectively
     narrow AND shift some subtree at the same time. */
  if (tree->root != ITREE_NULL)
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
          if (node->right != ITREE_NULL)
            {
              if (node->begin > pos)
                {
                  /* All nodes in this subtree are shifted by length. */
                  node->right->offset += length;
                  ++tree->otick;
                }
              else
                interval_stack_push (stack, node->right);
            }
          if (node->left != ITREE_NULL
              && pos <= node->left->limit + node->left->offset)
            interval_stack_push (stack, node->left);

          /* node->begin == pos implies no front-advance. */
          if (node->begin > pos)
            node->begin += length;
          if (node->end > pos || (node->end == pos && node->rear_advance))
            {
              node->end += length;
              eassert (node != ITREE_NULL);
              interval_tree_propagate_limit (node);
            }
        }
      interval_stack_destroy (stack);
    }

  /* Reinsert nodes starting at POS having front-advance. */
  nodeptr_and_flag nav;
  while ((nav = interval_stack_pop (saved),
          node = nav_nodeptr (nav)))
    {
      node->begin += length;
      if (node->end != pos || node->rear_advance)
        node->end += length;
      interval_tree_insert (tree, node);
    }

  interval_stack_destroy (saved);
}

/* Delete a gap at POS of length LENGTH, contracting all intervals
   intersecting it. */

void
interval_tree_delete_gap (struct interval_tree *tree, ptrdiff_t pos, ptrdiff_t length)
{
  if (length <= 0 || tree->root == ITREE_NULL)
    return;

  /* FIXME: Don't allocate stack anew every time. */

  /* Can't use the generator here, because by decrementing begin, we
     might unintentionally bring shifted nodes back into our search
     space. */
  const int size = interval_tree_max_height (tree) + 1;
  struct interval_stack *stack = interval_stack_create (size);
  struct interval_node *node;

  interval_stack_push (stack, tree->root);
  nodeptr_and_flag nav;
  while ((nav = interval_stack_pop (stack)))
    {
      node = nav_nodeptr (nav);
      interval_tree_inherit_offset (tree->otick, node);
      if (node->right != ITREE_NULL)
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
      if (node->left != ITREE_NULL
          && pos <= node->left->limit + node->left->offset)
        interval_stack_push (stack, node->left);

      if (pos < node->begin)
        node->begin = max (pos, node->begin - length);
      if (node->end > pos)
        {
          node->end = max (pos , node->end - length);
          eassert (node != ITREE_NULL);
          interval_tree_propagate_limit (node);
        }
    }
  interval_stack_destroy (stack);
}



/* +===================================================================================+
 * | Generator
 * +===================================================================================+ */

/* Allocate a new generator for TREE. */

static struct interval_generator *
interval_generator_create (struct interval_tree *tree)
{
  struct interval_generator *g = xmalloc (sizeof *g);
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

/* Return true, if NODE's interval intersects with [BEGIN, END).
   Note: We always include empty nodes at BEGIN (and not at END),
   but if BEGIN==END, then we don't include non-empty nodes starting
   at BEGIN or ending at END.  This seems to match the behavior of the
   old overlays code but it's not clear if it's The Right Thing
   (e.g. it breaks the expectation that if NODE1 is included, then
   a NODE2 strictly bigger than NODE1 should also be included).  */

static inline bool
interval_node_intersects (const struct interval_node *node,
                          ptrdiff_t begin, ptrdiff_t end)
{
  return (begin < node->end && node->begin < end)
    || (node->begin == node->end && begin == node->begin);
}

/* Return the next node of the iterator in the order given when it was
   started; or NULL if there are no more nodes. */

inline struct interval_node*
interval_generator_next (struct interval_generator *g)
{
  eassert (g->running);

  struct interval_node * const null = ITREE_NULL;
  struct interval_node *node;

  /* The `visited` flag stored in each node is used here (and only here):
     We keep a "workstack" of nodes we need to consider.  This stack
     consist of nodes of two types: nodes that we have decided
     should be returned by the generator, and nodes which we may
     need to consider (including checking their children).
     We start an iteration with a stack containing just the root
     node marked as "not visited" which means that it (and its children)
     needs to be considered but we haven't yet decided whether it's included
     in the generator's output.  */

  do {
    nodeptr_and_flag nav;
    bool visited;
    while ((nav = interval_stack_pop (g->stack),
            node = nav_nodeptr (nav),
            visited = nav_flag (nav),
            node && !visited))
      {
        struct interval_node * const left = node->left;
        struct interval_node * const right = node->right;

        interval_tree_inherit_offset (g->otick, node);
        eassert (itree_limit_is_stable (node));
        switch (g->order)
          {
          case ITREE_ASCENDING:
            if (right != null && node->begin <= g->end)
              interval_stack_push_flagged (g->stack, right, false);
            if (interval_node_intersects (node, g->begin, g->end))
              interval_stack_push_flagged (g->stack, node, true);
            /* Node's children may still be off-set and we need to add it. */
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
    /* Node may have been invalidated by interval_generator_narrow
       after it was pushed: Check if it still intersects. */
  } while (node && ! interval_node_intersects (node, g->begin, g->end));

  return node;
}

/* Limit G to the new interval [BEGIN, END), which must be a subset of
   the current one.  I.E. it can't grow on either side. */

inline void
interval_generator_narrow (struct interval_generator *g,
                           ptrdiff_t begin, ptrdiff_t end)
{
  eassert (g->running);
  eassert (begin >= g->begin);
  eassert (end <= g->end);
  g->begin =  max (begin, g->begin);
  g->end =  min (end, g->end);
}


/* +===================================================================================+
 * | Internal Functions
 * +===================================================================================+ */

/* Update NODE's limit attribute according to its children. */

static void
interval_tree_update_limit (struct interval_node *node)
{
  if (node == ITREE_NULL)
    return;

  node->limit = itree_newlimit (node);
}

/* Apply NODE's offset to its begin, end and limit values and
   propagate it to its children.

   Does nothing, if NODE is clean, i.e. NODE.otick = tree.otick .
*/

static void
interval_tree_inherit_offset (uintmax_t otick, struct interval_node *node)
{
  if (node->otick == otick)
    {
      eassert (node->offset == 0);
      return;
    }

  if (node->offset)
    {
      node->begin += node->offset;
      node->end   += node->offset;
      node->limit += node->offset;
      if (node->left != ITREE_NULL)
        node->left->offset += node->offset;
      if (node->right != ITREE_NULL)
        node->right->offset += node->offset;
      node->offset = 0;
    }
  /* FIXME: I wonder when/why this condition can be false, and more
     generally why we'd want to propagate offsets that may not be
     fully up-to-date. --stef

     Offsets can be inherited from dirty nodes (with out of date
     otick) during insert and remove.  Offsets aren't inherited
     downward from the root for these operations so rotations are
     performed on potentially "dirty" nodes.  We could fix this by
     always inheriting offsets downward from the root for every insert
     and remove.  --matt
  */
  if (node->parent == ITREE_NULL || node->parent->otick == otick)
    node->otick = otick;
}

/* Update limit of NODE and its ancestors.  Stop when it becomes
   stable, i.e. new_limit = old_limit.

   NODE may also be the null node, in which case its parent is
   used. (This feature is due to the RB algorithm.)
*/

static void
interval_tree_propagate_limit (struct interval_node *node)
{
  if (node == ITREE_NULL)
    return;

  while (1) {
    ptrdiff_t newlimit = itree_newlimit (node);
    if (newlimit == node->limit)
      break;
    node->limit = newlimit;
    if (node->parent == ITREE_NULL)
      break;
    node = node->parent;
  }
}

/* Perform the familiar left-rotation on node NODE. */

static void
interval_tree_rotate_left (struct interval_tree *tree, struct interval_node *node)
{
  eassert (node->right != ITREE_NULL);

  struct interval_node *right = node->right;

  interval_tree_inherit_offset (tree->otick, node);
  interval_tree_inherit_offset (tree->otick, right);

  /* Turn right's left subtree into node's right subtree.  */
  node->right = right->left;
  if (right->left != ITREE_NULL)
    right->left->parent = node;

  /* right's parent was node's parent.  */
  if (right != ITREE_NULL)
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
  if (node != ITREE_NULL)
    node->parent = right;

  /* Order matters here. */
  interval_tree_update_limit (node);
  interval_tree_update_limit (right);
}

/* Perform the familiar right-rotation on node NODE. */

static void
interval_tree_rotate_right (struct interval_tree *tree, struct interval_node *node)
{
  eassert (tree && node && node->left != ITREE_NULL);

  struct interval_node *left = node->left;

  interval_tree_inherit_offset (tree->otick, node);
  interval_tree_inherit_offset (tree->otick, left);

  node->left = left->right;
  if (left->right != ITREE_NULL)
    left->right->parent = node;

  if (left != ITREE_NULL)
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
  if (node != ITREE_NULL)
    node->parent = left;

  interval_tree_update_limit (left);
  interval_tree_update_limit (node);
}

/* Repair the tree after an insertion.
   The new NODE was added as red, so we may have 2 reds in a row.
   Rebalance the parents as needed to re-establish the RB invariants. */

static void
interval_tree_insert_fix (struct interval_tree *tree, struct interval_node *node)
{
  while (node->parent->red)
    {
      /* NODE is red and its parent is red.  This is a violation of
	 red-black tree property #3.  */
      eassert (node->red);

      if (node->parent == node->parent->parent->left)
	{
	  /* We're on the left side of our grandparent, and OTHER is
	     our "uncle".  */
	  struct interval_node *uncle = node->parent->parent->right;

	  if (uncle->red) /* case 1.a */
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
	  struct interval_node *uncle = node->parent->parent->left;

	  if (uncle->red) /* case 1.b */
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

  /* The root may have been changed to red due to the algorithm.  Set
     it to black so that property #5 is satisfied.  */
  tree->root->red = false;
  eassert (check_tree (tree));
}

/* Return accumulated offsets of NODE's parents.  */
static ptrdiff_t
itree_total_offset (struct interval_node *node)
{
  eassert (node != ITREE_NULL);
  ptrdiff_t offset = 0;
  while (node->parent != ITREE_NULL)
    {
      node = node->parent;
      offset += node->offset;
    }
  return offset;
}

/* Link node SOURCE in DEST's place.
   It's the caller's responsability to refresh the `limit`s
   of DEST->parents afterwards.  */

static void
interval_tree_transplant (struct interval_tree *tree, struct interval_node *source,
                          struct interval_node *dest)
{
  eassert (tree && source && dest && dest != ITREE_NULL);
  eassert (source == ITREE_NULL
           || itree_total_offset (source) == itree_total_offset (dest));

  if (dest == tree->root)
    tree->root = source;
  else if (dest == dest->parent->left)
    dest->parent->left = source;
  else
    dest->parent->right = source;

  source->parent = dest->parent;
}


/* +===================================================================================+
 * | Debugging
 * +===================================================================================+ */

/* See Foverlay_tree in buffer.c */
