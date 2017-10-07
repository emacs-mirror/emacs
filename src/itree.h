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

#ifndef ITREE_H
#define ITREE_H
#include <config.h>
#include <stddef.h>
#include <inttypes.h>

/* The tree and node structs are mainly here, so they can be allocated.

   NOTE: The only time where it is safe to modify node.begin and
   node.end directly, is while the node is not part of any tree.

   NOTE: It is safe to read node.begin and node.end directly, if the
   node came from a generator, because it validates the nodes it
   returns as a side-effect.
*/

struct interval_node;
struct interval_node
{
  struct interval_node *parent;
  struct interval_node *left;
  struct interval_node *right;
  ptrdiff_t begin;		/* The beginning of this interval. */
  ptrdiff_t end;		/* The end of the interval. */
  ptrdiff_t limit;		/* The maximum end in this subtree. */
  ptrdiff_t offset;		/* The amount of shift to apply to this subtree. */
  uintmax_t otick;              /* offset modified tick */
  Lisp_Object data;             /* Exclusively used by the client. */
  enum { ITREE_RED, ITREE_BLACK } color;
  bool_bf visited : 1;          /* For traversal via generator. */
  bool_bf rear_advance : 1;     /* Same as for marker and overlays.  */
  bool_bf front_advance : 1;    /* Same as for marker and overlays.  */
};

struct interval_tree
{
  struct interval_node *root;
  struct interval_node nil;	/* The tree's version of NULL. */
  uintmax_t otick;              /* offset tick, compared with node's otick. */
  intmax_t size;                /* Number of nodes in the tree. */
  struct interval_generator *iter;
  bool_bf iter_running;
};

enum interval_tree_order {
  ITREE_ASCENDING = 0,
  ITREE_DEFLT_ORDER = 0,
  ITREE_DESCENDING,
  ITREE_PRE_ORDER,
};

void interval_node_init(struct interval_node *, ptrdiff_t, ptrdiff_t, bool, bool, Lisp_Object);
ptrdiff_t interval_node_begin(struct interval_tree *, struct interval_node *);
ptrdiff_t interval_node_end(struct interval_tree *, struct interval_node *);
void interval_node_set_region(struct interval_tree *, struct interval_node *, ptrdiff_t, ptrdiff_t);
struct interval_tree *interval_tree_create(void);
void interval_tree_destroy(struct interval_tree *);
intmax_t interval_tree_size(struct interval_tree *);
void interval_tree_clear(struct interval_tree *);
void interval_tree_insert(struct interval_tree *, struct interval_node *);
bool interval_tree_contains(struct interval_tree *, struct interval_node *);
struct interval_node *interval_tree_remove(struct interval_tree *, struct interval_node *);
void interval_tree_iter_start(struct interval_tree *, ptrdiff_t, ptrdiff_t, enum interval_tree_order);
void interval_tree_iter_narrow(struct interval_tree *, ptrdiff_t, ptrdiff_t);
void interval_tree_iter_finish(struct interval_tree *);
struct interval_node *interval_tree_iter_next(struct interval_tree *);
void interval_tree_insert_gap(struct interval_tree *, ptrdiff_t, ptrdiff_t);
void interval_tree_delete_gap(struct interval_tree *, ptrdiff_t, ptrdiff_t);
void interval_tree_nodes (struct interval_tree *tree, struct interval_node **nodes, enum interval_tree_order order);
#endif
