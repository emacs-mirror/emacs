/* This file implements an efficient interval data-structure.

Copyright (C) 2017 Andreas Politz (politza@hochschule-trier.de)

This file is not part of GNU Emacs.

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
#include "lisp.h"
#include "itree.h"

/*
   Intervals of the form [BEGIN, END), are stored as nodes inside a RB
   tree, sorted by BEGIN .  The core operation of this tree (besides
   insert, remove, etc.) is finding all intervals intersecting with
   some given interval.  In order to perform this operation
   efficiently, every node stores a third value called LIMIT. (See
   https://en.wikipedia.org/wiki/Interval_tree#Augmented_tree and its
   source Introduction to Algorithms (Section 14.3), Cormen et al. .)

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

   ==== Adjusting intervals ====

   Since this data-structure will be used for overlays in an Emacs
   buffer, a second core operation implements the ability to insert or
   delete gaps in resp. from the tree.  This models the insertion
   resp. deletion of text in a buffer and the effects it may have on
   the positions of overlays.

   Consider this: Something gets inserted at position P into a buffer
   and assume that all overlays occur strictly after P.  Ordinarily,
   we would have to iterate all overlays and increment their BEGIN and
   END values accordingly (the insertion of text pushes them back).
   In order to avoid this, we introduce yet another node attribute,
   called OFFSET.

   The OFFSET of some some subtree, represented by its root, is the
   amount of shift that needs to be applied to its BEGIN, END and
   LIMIT values, in order to get to the real values.  Coming back to
   the example, all we would need to do in this case, is to increment
   the OFFSET of the tree's root, without any traversal of the tree
   itself.

   As a consequence, the real values of BEGIN, END and LIMIT of some
   NODE need to be computed by incrementing them by the sum of NODE's
   OFFSET and all of its ancestors offsets.  Therefore, we store a
   counter (otick) inside every node and also the tree, by which we
   remember the fact, that a node's path to the root has no offsets
   applied (i.e. its values are up to date).  This is the case if some
   node's value differs from the tree's one, the later of which is
   incremented whenever some node's offset has changed.
*/

static struct interval_node *interval_tree_validate(struct interval_tree *, struct interval_node *);
static void interval_generator_ensure_space(struct interval_generator *);
static bool interval_node_intersects(const struct interval_node *, ptrdiff_t, ptrdiff_t);
static int interval_tree_max_height(const struct interval_tree *);
static struct interval_stack *interval_stack_create(intmax_t);
static void interval_stack_destroy(struct interval_stack *);
static void interval_stack_clear(struct interval_stack *);
static void interval_stack_ensure_space(struct interval_stack *, intmax_t);
static void interval_stack_push(struct interval_stack *, struct interval_node *);
static void interval_stack_push_flagged(struct interval_stack *, struct interval_node *, bool);
static struct interval_node *interval_stack_pop(struct interval_stack *);
static void interval_tree_update_limit(const struct interval_tree *, struct interval_node *);
static void interval_tree_inherit_offset(const struct interval_tree *, struct interval_node *);
static void interval_tree_propagate_limit(const struct interval_tree *, struct interval_node *);
static void interval_tree_rotate_left(struct interval_tree *, struct interval_node *);
static void interval_tree_rotate_right(struct interval_tree *, struct interval_node *);
static void interval_tree_insert_fix(struct interval_tree *, struct interval_node *);
static void interval_tree_remove_fix(struct interval_tree *, struct interval_node *);
static void interval_tree_transplant(struct interval_tree *, struct interval_node *, struct interval_node *);
static struct interval_node *interval_tree_subtree_min(const struct interval_tree *, struct interval_node *);
static struct interval_generator* interval_generator_create (struct interval_tree *);
static void interval_generator_destroy (struct interval_generator *);
static void interval_generator_reset (struct interval_generator *,
                               ptrdiff_t, ptrdiff_t,
                               enum interval_tree_order);
static void
interval_generator_narrow (struct interval_generator *g,
                             ptrdiff_t begin, ptrdiff_t end);
static inline struct interval_node*
interval_generator_next (struct interval_generator *g);
static inline void interval_tree_iter_ensure_space(struct interval_tree *);



/* +------------------------------------------------------------------------------------+ */

/* Simple dynamic array. */
struct interval_stack
{
  struct interval_node **nodes;
  size_t size;
  size_t length;
};

/* State used when iterating interval. */
struct interval_generator
{
  struct interval_tree *tree;
  struct interval_stack *stack;
  ptrdiff_t begin;
  ptrdiff_t end;
  enum interval_tree_order order;
};



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
      interval_tree_propagate_limit (tree, node);
    }
}

/* Allocate an interval_tree. Free with interval_tree_destroy. */

struct interval_tree*
interval_tree_create (void)
{
  struct interval_tree *tree = xmalloc (sizeof (*tree));
  interval_tree_clear (tree);
  tree->iter = interval_generator_create (tree);
  return tree;
}

/* Reset the tree TREE to its empty state. */

void
interval_tree_clear (struct interval_tree *tree)
{
  struct interval_node *nil = &tree->nil;
  nil->left = nil->right = nil->parent = nil;
  nil->offset = nil->otick = 0;
  nil->begin = PTRDIFF_MIN;
  nil->end = PTRDIFF_MIN;
  nil->limit = PTRDIFF_MIN;     /* => max(x, nil.limit) = x */
  nil->color = ITREE_BLACK;
  tree->root = nil;
  tree->otick = 1;
  tree->size = 0;
  tree->iter_running = 0;
}

#ifdef ITREE_TESTING
/* Initialize a pre-allocated tree (presumably on the stack). */

static void
interval_tree_init (struct interval_tree *tree)
{
  interval_tree_clear (tree);
  tree->iter = interval_generator_create (tree);
}
#endif

/* Release a tree, freeing its allocated memory. */
void
interval_tree_destroy (struct interval_tree *tree)
{
  if (! tree)
    return;
  if (tree->iter)
    interval_generator_destroy (tree->iter);
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
  eassert (node && node->begin <= node->end && node != &tree->nil);

  struct interval_node *parent = &tree->nil;
  struct interval_node *child = tree->root;
  ptrdiff_t offset = 0;

  /* Find the insertion point, accumulate node's offset and update
     ancestors limit values. */
  while (child != &tree->nil)
    {
      parent = child;
      offset += child->offset;
      child->limit = max (child->limit, node->end - offset);
      /* This suggests that nodes in the right subtree are strictly
         greater.  But this is not true due to later rotations. */
      child = node->begin <= child->begin ? child->left : child->right;
    }

  /* Insert the node */
  if (parent == &tree->nil)
    tree->root = node;
  else if (node->begin <= parent->begin)
    parent->left = node;
  else
    parent->right = node;

  /* Init the node */
  node->parent = parent;
  node->left = &tree->nil;
  node->right = &tree->nil;
  node->color = ITREE_RED;
  node->offset = 0;
  node->begin -= offset;
  node->end -= offset;
  node->limit = node->end;
  node->otick = tree->otick - 1;

  /* Fix/update the tree */
  ++tree->size;
  interval_tree_insert_fix (tree, node);
  interval_tree_iter_ensure_space (tree);
}

/* Return true, if NODE is a member of TREE. */

bool
interval_tree_contains (struct interval_tree *tree, struct interval_node *node)
{
  struct interval_node *other;

  interval_tree_iter_start (tree, node->begin, PTRDIFF_MAX, ITREE_ASCENDING);
  while ((other = interval_tree_iter_next (tree)))
    if (other == node)
      break;

  interval_tree_iter_finish (tree);
  return other == node;
}

/* Remove NODE from TREE and return it. NODE must exist in TREE.*/

struct interval_node*
interval_tree_remove (struct interval_tree *tree, struct interval_node *node)
{
  eassert (interval_tree_contains (tree, node));

  struct interval_node *broken = NULL;

  interval_tree_inherit_offset (tree, node);
  if (node->left == &tree->nil || node->right == &tree->nil)
    {
      struct interval_node *subst =
        (node->right == &tree->nil) ? node->left : node->right;
      if (node->color == ITREE_BLACK)
        broken = subst;
      interval_tree_transplant (tree, subst, node);
      interval_tree_propagate_limit (tree, subst);
    }
  else
    {
      struct interval_node *min = interval_tree_subtree_min (tree, node->right);
      struct interval_node *min_right = min->right;

      if (min->color == ITREE_BLACK)
        broken = min->right;
      if (min->parent == node)
        min_right->parent = min; /* set parent, if min_right = nil */
      else
        {
          interval_tree_transplant (tree, min->right, min);
          min->right = node->right;
          min->right->parent = min;
        }
      interval_tree_inherit_offset (tree, min);
      interval_tree_transplant (tree, min, node);
      min->left = node->left;
      min->left->parent = min;
      min->color = node->color;
      interval_tree_propagate_limit (tree, min_right);
      interval_tree_propagate_limit (tree, min);
    }

  if (broken)
    interval_tree_remove_fix (tree, broken);

  node->right = node->left = node->parent = NULL;
  --tree->size;

  eassert (tree->size == 0 || (tree->size > 0 && tree->root != &tree->nil));

  return node;
}

static struct interval_node*
interval_tree_validate (struct interval_tree *tree, struct interval_node *node)
{

  if (tree->otick == node->otick || node == &tree->nil)
    return node;
  if (node != tree->root)
    interval_tree_validate (tree, node->parent);

  interval_tree_inherit_offset (tree, node);
  return node;
}

/* Start a generator iterating all intervals in [BEGIN,END) in the
   given ORDER. Only one iterator per tree can be running at any
   time.
*/

void
interval_tree_iter_start (struct interval_tree *tree,
                          ptrdiff_t begin, ptrdiff_t end,
                          enum interval_tree_order order)
{
  if (tree->iter_running)
    emacs_abort ();
  interval_generator_reset (tree->iter, begin, end, order);
  tree->iter_running = 1;
}

/* Limit the search interval of the iterator to the given values.  The
   interval can only shrink, but never grow.*/

inline void
interval_tree_iter_narrow(struct interval_tree *tree,
                               ptrdiff_t begin, ptrdiff_t end)
{
  if (! tree->iter_running)
    emacs_abort ();
  interval_generator_narrow (tree->iter, begin, end);
}

/* Stop using the iterator. */

void
interval_tree_iter_finish (struct interval_tree *tree)
{
  if (! tree->iter_running)
    emacs_abort ();
  tree->iter_running = 0;
}

/* Return the next node of the iterator in the order given when it was
   started; or NULL if there are no more nodes. */

inline struct interval_node*
interval_tree_iter_next (struct interval_tree *tree)
{
  if (! tree->iter_running)
    emacs_abort ();
  return interval_generator_next (tree->iter);
}

/* Ensure that the tree's iterator does not need to allocate space
   until the tree grows in size. */

static inline void
interval_tree_iter_ensure_space (struct interval_tree *tree)
{
  interval_generator_ensure_space (tree->iter);
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
  if (length <= 0 || tree->size == 0)
    return;

  /* FIXME: Don't allocate generator/stack anew every time. */

  /* Nodes with front_advance starting at pos may mess up the tree
     order, so we need to remove them first. */
  struct interval_stack *saved = interval_stack_create (0);
  struct interval_node *node = NULL;
  interval_tree_iter_start (tree, pos, pos + 1, ITREE_PRE_ORDER);
  while ((node = interval_tree_iter_next (tree)))
    {
      if (node->begin == pos && node->front_advance
          && (node->begin != node->end || node->rear_advance))
        interval_stack_push (saved, node);
    }
  interval_tree_iter_finish (tree);
  for (int i = 0; i < saved->length; ++i)
    interval_tree_remove (tree, saved->nodes[i]);


  /* We can't use a generator here, because we can't effectively
     narrow AND shift some subtree at the same time. */
  const int size = interval_tree_max_height (tree) + 1;
  struct interval_stack *stack = interval_stack_create (size);
  interval_stack_push (stack, tree->root);
  while ((node = interval_stack_pop (stack)))
    {
      /* Process in pre-order. */
      interval_tree_inherit_offset (tree, node);
      if (node->right != &tree->nil)
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
      if (node->left != &tree->nil
          && pos <= node->left->limit + node->left->offset)
        interval_stack_push (stack, node->left);

      /* node->begin == pos implies no front-advance. */
      if (node->begin > pos)
        node->begin += length;
      if (node->end > pos || (node->end == pos && node->rear_advance))
        {
          node->end += length;
          interval_tree_propagate_limit (tree, node);
        }
    }
  interval_stack_destroy (stack);

  /* Reinsert nodes starting at POS having front-advance. */
  while ((node = interval_stack_pop (saved)))
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
  if (length <= 0 || tree->size == 0)
    return;

  /* FIXME: Don't allocate stack anew every time. */

  /* Can't use the generator here, because by decrementing begin, we
     might unintentionally bring shifted nodes back into our search
     space. */
  const int size = interval_tree_max_height (tree) + 1;
  struct interval_stack *stack = interval_stack_create (size);
  struct interval_node *node;

  interval_stack_push (stack, tree->root);
  while ((node = interval_stack_pop (stack)))
    {
      interval_tree_inherit_offset (tree, node);
      if (node->right != &tree->nil)
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
      if (node->left != &tree->nil
          && pos <= node->left->limit + node->left->offset)
        interval_stack_push (stack, node->left);

      if (pos < node->begin)
        node->begin = max (pos, node->begin - length);
      if (node->end > pos)
        {
          node->end = max (pos , node->end - length);
          interval_tree_propagate_limit (tree, node);
        }
    }
  interval_stack_destroy (stack);
}



/* +===================================================================================+
 * | Generator
 * +===================================================================================+ */

/* Allocate a new generator for TREE. */

static struct interval_generator*
interval_generator_create  (struct interval_tree *tree)
{
  struct interval_generator *g = xmalloc (sizeof *g);
  const int size = interval_tree_max_height (tree) + 1;

  g->stack = interval_stack_create (size);
  g->tree = tree;
  interval_generator_reset (g, 1, 0, 0);
  return g;
}

/* Reset generator G such that it iterates over intervals intersecting
   with [BEGIN, END) in the given ORDER. */

void
interval_generator_reset (struct interval_generator *g,
                          ptrdiff_t begin, ptrdiff_t end,
                          enum interval_tree_order order)
{
  if (! g) return;

  g->begin = begin;
  g->end = end;
  g->order = order;
  interval_stack_clear (g->stack);
  if (begin <= end  && g->tree->size > 0)
    interval_stack_push_flagged (g->stack, g->tree->root, false);
}

/* Allocate enough space for the tree of G in its current shape. */

static inline void
interval_generator_ensure_space (struct interval_generator *g)
{
  interval_stack_ensure_space (g->stack, interval_tree_max_height (g->tree) + 1);
}

/* Return true, if NODE's interval intersects with [BEGIN, END). */

static inline bool
interval_node_intersects (const struct interval_node *node,
                          ptrdiff_t begin, ptrdiff_t end)
{
  return (begin < node->end && node->begin < end)
    || (node->begin == node->end && begin == node->begin);
}

/* Return the next node of G, or NULL if there is none. */

inline struct interval_node*
interval_generator_next (struct interval_generator *g)
{
  if (! g) return NULL;

  struct interval_node * const nil = &g->tree->nil;
  struct interval_node *node;

  do {
    node = interval_stack_pop (g->stack);

    while (node && ! node->visited)
      {
        struct interval_node * const left = node->left;
        struct interval_node * const right = node->right;

        interval_tree_inherit_offset (g->tree, node);
        switch (g->order)
          {
          case ITREE_ASCENDING:
            if (right != nil && node->begin <= g->end)
              interval_stack_push_flagged (g->stack, right, false);
            if (interval_node_intersects (node, g->begin, g->end))
              interval_stack_push_flagged (g->stack, node, true);
            /* Node's children may still be off-set and we need to add it. */
            if (left != nil && g->begin <= left->limit + left->offset)
              interval_stack_push_flagged (g->stack, left, false);
            break;
          case ITREE_DESCENDING:
            if (left != nil && g->begin <= left->limit + left->offset)
              interval_stack_push_flagged (g->stack, left, false);
            if (interval_node_intersects (node, g->begin, g->end))
              interval_stack_push_flagged (g->stack, node, true);
            if (right != nil && node->begin <= g->end)
              interval_stack_push_flagged (g->stack, right, false);
            break;
          case ITREE_PRE_ORDER:
            if (right != nil && node->begin <= g->end)
              interval_stack_push_flagged (g->stack, right, false);
            if (left != nil && g->begin <= left->limit + left->offset)
              interval_stack_push_flagged (g->stack, left, false);
            if (interval_node_intersects (node, g->begin, g->end))
              interval_stack_push_flagged (g->stack, node, true);
            break;
          }
        node = interval_stack_pop (g->stack);
      }
    /* Node may have been invalidated by interval_generator_narrow
       after it was pushed: Check if it still intersects. */
  } while (node && ! interval_node_intersects (node, g->begin, g->end));

  return node;
}

/* Limit G to the new interval [BEGIN, END), which must be a subset of
   the current one.  I.E. it can't grow on either side. */

static inline void
interval_generator_narrow (struct interval_generator *g,
                           ptrdiff_t begin, ptrdiff_t end)
{
  g->begin =  max (begin, g->begin);
  g->end =  min (end, g->end);
}

/* Free the memory allocated for G. */

void
interval_generator_destroy (struct interval_generator *g)
{
  if (! g) return;
  if (g->stack)
    interval_stack_destroy (g->stack);
  xfree (g);
}


/* +===================================================================================+
 * | Stack
 * +===================================================================================+ */

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
      stack->nodes = xrealloc (stack->nodes, stack->size * sizeof (*stack->nodes));
    }
}

static inline void
interval_stack_push (struct interval_stack *stack, struct interval_node *node)
{
  interval_stack_ensure_space (stack, stack->length + 1);
  stack->nodes[stack->length] = node;
  stack->length++;
}

/* Push NODE on the STACK, while settings its visited flag to FLAG. */

static inline void
interval_stack_push_flagged (struct interval_stack *stack,
                             struct interval_node *node, bool flag)
{
  interval_stack_push (stack, node);
  node->visited = flag;
}

static inline struct interval_node*
interval_stack_pop (struct interval_stack *stack)
{
  if (stack->length == 0)
    return NULL;
  return stack->nodes[--stack->length];
}


/* +===================================================================================+
 * | Internal Functions
 * +===================================================================================+ */

/* Update NODE's limit attribute according to its children. */

static void
interval_tree_update_limit (const struct interval_tree *tree,
                            struct interval_node *node)
{
  if (node == &tree->nil)
    return;

  node->limit = max (node->end, max (node->left->limit + node->left->offset,
                                     node->right->limit + node->right->offset));
}

/* Apply NODE's offset to its begin, end and limit values and
   propagate it to its children.

   Does nothing, if NODE is clean, i.e. NODE.otick = tree.otick .
*/

static void
interval_tree_inherit_offset (const struct interval_tree *tree,
                              struct interval_node *node)
{

  if (node->otick == tree->otick)
    return;

  node->begin += node->offset;
  node->end += node->offset;
  node->limit += node->offset;
  if (node->left != &tree->nil)
    node->left->offset += node->offset;
  if (node->right != &tree->nil)
    node->right->offset += node->offset;
  node->offset = 0;
  if (node == tree->root || node->parent->otick == tree->otick)
    node->otick = tree->otick;
}

/* Update limit of NODE and its ancestors.  Stop when it becomes
   stable, i.e. new_limit = old_limit.

   NODE may also be the nil node, in which case its parent is
   used. (This feature is due to the RB algorithm.)
*/

static void
interval_tree_propagate_limit (const struct interval_tree *tree,
                               struct interval_node *node)
{
  if (node == &tree->nil)
    node = node->parent;
  if (node == &tree->nil)
    return;

  while (1) {
    ptrdiff_t newlimit = max (node->end, max (node->left->limit + node->left->offset,
                                              node->right->limit + node->right->offset));
    if (newlimit == node->limit)
      break;
    node->limit = newlimit;
    if (node == tree->root)
      break;
    node = node->parent;
  }
}

/* Perform the familiar left-rotation on node NODE. */

static void
interval_tree_rotate_left (struct interval_tree *tree, struct interval_node *node)
{
  eassert (node->right != &tree->nil);

  struct interval_node *right = node->right;

  interval_tree_inherit_offset (tree, node);
  interval_tree_inherit_offset (tree, right);

  /* Turn right's left subtree into node's right subtree.  */
  node->right = right->left;
  if (right->left != &tree->nil)
    right->left->parent = node;

  /* right's parent was node's parent.  */
  if (right != &tree->nil)
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
  if (node != &tree->nil)
    node->parent = right;

  /* Order matters here. */
  interval_tree_update_limit (tree, node);
  interval_tree_update_limit (tree, right);
}

/* Perform the familiar right-rotation on node NODE. */

static void
interval_tree_rotate_right (struct interval_tree *tree, struct interval_node *node)
{
  eassert (tree && node && node->left != &tree->nil);

  struct interval_node *left = node->left;

  interval_tree_inherit_offset (tree, node);
  interval_tree_inherit_offset (tree, left);

  node->left = left->right;
  if (left->right != &tree->nil)
    left->right->parent = node;

  if (left != &tree->nil)
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
  if (node != &tree->nil)
    node->parent = left;

  interval_tree_update_limit (tree, left);
  interval_tree_update_limit (tree, node);
}

/* Repair the tree after an insertion.  Part of the RB-Tree
   algorithm. */

static void
interval_tree_insert_fix (struct interval_tree *tree, struct interval_node *node)
{
  while (node->parent->color == ITREE_RED)
    {
      /* NODE is red and its parent is red.  This is a violation of
	 red-black tree property #3.  */

      if (node->parent == node->parent->parent->left)
	{
	  /* We're on the left side of our grandparent, and OTHER is
	     our "uncle".  */
	  struct interval_node *uncle = node->parent->parent->right;

	  if (uncle->color == ITREE_RED) /* case 1.a */
	    {
	      /* Uncle and parent are red but should be black because
		 NODE is red.  Change the colors accordingly and
		 proceed with the grandparent.  */
	      node->parent->color = ITREE_BLACK;
	      uncle->color = ITREE_BLACK;
	      node->parent->parent->color = ITREE_RED;
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
	      node->parent->color = ITREE_BLACK;
	      node->parent->parent->color = ITREE_RED;
	      interval_tree_rotate_right (tree, node->parent->parent);
            }
        }
      else
	{
	  /* This is the symmetrical case of above.  */
	  struct interval_node *uncle = node->parent->parent->left;

	  if (uncle->color == ITREE_RED) /* case 1.b */
	    {
	      node->parent->color = ITREE_BLACK;
	      uncle->color = ITREE_BLACK;
	      node->parent->parent->color = ITREE_RED;
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
	      node->parent->color = ITREE_BLACK;
	      node->parent->parent->color = ITREE_RED;
	      interval_tree_rotate_left (tree, node->parent->parent);
            }
        }
    }

  /* The root may have been changed to red due to the algorithm.  Set
     it to black so that property #5 is satisfied.  */
  tree->root->color = ITREE_BLACK;
}

/* Repair the tree after a deletion.  Part of the RB-Tree
   algorithm. */

static void
interval_tree_remove_fix (struct interval_tree *tree, struct interval_node *node)
{
  while (node != tree->root && node->color == ITREE_BLACK)
    {
      if (node == node->parent->left)
	{
	  struct interval_node *other = node->parent->right;

	  if (other->color == ITREE_RED) /* case 1.a */
	    {
	      other->color = ITREE_BLACK;
	      node->parent->color = ITREE_RED;
	      interval_tree_rotate_left (tree, node->parent);
	      other = node->parent->right;
            }

	  if (other->left->color == ITREE_BLACK /* 2.a */
              && other->right->color == ITREE_BLACK)
	    {
	      other->color = ITREE_RED;
	      node = node->parent;
            }
	  else
	    {
	      if (other->right->color == ITREE_BLACK) /* 3.a */
		{
		  other->left->color = ITREE_BLACK;
		  other->color = ITREE_RED;
		  interval_tree_rotate_right (tree, other);
		  other = node->parent->right;
                }
	      other->color = node->parent->color; /* 4.a */
	      node->parent->color = ITREE_BLACK;
	      other->right->color = ITREE_BLACK;
	      interval_tree_rotate_left (tree, node->parent);
	      node = tree->root;
            }
        }
      else
	{
	  struct interval_node *other = node->parent->left;

	  if (other->color == ITREE_RED) /* 1.b */
	    {
	      other->color = ITREE_BLACK;
	      node->parent->color = ITREE_RED;
	      interval_tree_rotate_right (tree, node->parent);
	      other = node->parent->left;
            }

	  if (other->right->color == ITREE_BLACK /* 2.b */
              && other->left->color == ITREE_BLACK)
	    {
	      other->color = ITREE_RED;
	      node = node->parent;
            }
	  else
	    {
	      if (other->left->color == ITREE_BLACK) /* 3.b */
		{
		  other->right->color = ITREE_BLACK;
		  other->color = ITREE_RED;
		  interval_tree_rotate_left (tree, other);
		  other = node->parent->left;
                }

	      other->color = node->parent->color; /* 4.b */
	      node->parent->color = ITREE_BLACK;
	      other->left->color = ITREE_BLACK;
	      interval_tree_rotate_right (tree, node->parent);
	      node = tree->root;
            }
        }
    }

  node->color = ITREE_BLACK;
}

/* Link node SOURCE in DEST's place. */

static void
interval_tree_transplant (struct interval_tree *tree, struct interval_node *source,
                          struct interval_node *dest)
{
  eassert (tree && source && dest && dest != &tree->nil);

  if (dest == tree->root)
    tree->root = source;
  else if (dest == dest->parent->left)
    dest->parent->left = source;
  else
    dest->parent->right = source;

  source->parent = dest->parent;
}


static struct interval_node*
interval_tree_subtree_min (const struct interval_tree *tree, struct interval_node *node)
{
  if (node == &tree->nil)
    return node;
  while (node->left != &tree->nil)
    node = node->left;
  return node;
}


/* +===================================================================================+
 * | Debugging
 * +===================================================================================+ */

/* See Foverlay_tree in buffer.c */
