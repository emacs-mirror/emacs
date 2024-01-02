/* This file implements an efficient interval data-structure.

Copyright (C) 2017-2024 Free Software Foundation, Inc.

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
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#ifndef ITREE_H
#define ITREE_H
#include <config.h>
#include <stddef.h>
#include <inttypes.h>

#include "lisp.h"

/* The tree and node structs are mainly here, so they can be
   allocated.

   NOTE: The only time where it is safe to modify node.begin and
   node.end directly, is while the node is not part of any tree.

   NOTE: It is safe to read node.begin and node.end directly, if the
   node came from an iterator, because it validates the nodes it
   returns as a side-effect.  See ITREE_FOREACH.
 */

struct itree_node
{
  /* The normal parent, left and right links found in binary trees.
     See also `red`, below, which completes the Red-Black tree
     representation.  */
  struct itree_node *parent;
  struct itree_node *left;
  struct itree_node *right;

  /* The following five fields comprise the interval abstraction.

     BEGIN, END are buffer positions describing the range.  When a
     node is in a tree these fields are read only, written only by
     itree functions.

     The LIMIT, OFFSET and OTICK fields should be considered internal
     to itree.c and used only by itree functions.

     LIMIT is a buffer position, the maximum of END of this node and
     its children.  See itree.c for its use.

     OFFSET is in buffer position units, and will be non-zero only
     when the node is dirty.

     OTICK determines whether BEGIN, END, LIMIT and OFFSET are
     considered dirty.  A node is clean when its OTICK is equal to the
     OTICK of its tree (see struct itree_tree).  Otherwise, it is
     dirty.

     In a clean node, BEGIN, END and LIMIT are correct buffer
     positions, and OFFSET is zero.  The parent of a clean node is
     also clean, recursively.

     In a dirty node, the node's OTICK won't equal its tree's OTICK,
     and its OFFSET may be non-zero.  At all times the descendents of
     a dirty node are also dirty.  BEGIN, END and LIMIT require
     adjustment before use as buffer positions.

     NOTE: BEGIN and END must not be modified while the node is part
     of a tree.  Use itree_insert_gap and itree_delete_gap instead.

     NOTE: The interval iterators ensure nodes are clean before
     yielding them, so BEGIN and END may be safely used as buffer
     positions then.  */

  ptrdiff_t begin;		/* The beginning of this interval. */
  ptrdiff_t end;		/* The end of the interval. */
  ptrdiff_t limit;		/* The maximum end in this subtree. */
  ptrdiff_t offset;		/* The amount of shift to apply to this subtree. */
  uintmax_t otick;              /* offset modified tick */
  Lisp_Object data;             /* Exclusively used by the client. */
  bool_bf red : 1;
  bool_bf rear_advance : 1;     /* Same as for marker and overlays.  */
  bool_bf front_advance : 1;    /* Same as for marker and overlays.  */
};

struct itree_tree
{
  struct itree_node *root;
  uintmax_t otick;              /* offset tick, compared with node's otick. */
  intmax_t size;                /* Number of nodes in the tree. */
};

enum itree_order
  {
    ITREE_ASCENDING,
    ITREE_DESCENDING,
    ITREE_PRE_ORDER,
    ITREE_POST_ORDER,
  };

extern void itree_node_init (struct itree_node *, bool, bool, Lisp_Object);
extern ptrdiff_t itree_node_begin (struct itree_tree *, struct itree_node *);
extern ptrdiff_t itree_node_end (struct itree_tree *, struct itree_node *);
extern void itree_node_set_region (struct itree_tree *, struct itree_node *,
				   ptrdiff_t, ptrdiff_t);
extern struct itree_tree *itree_create (void);
extern void itree_destroy (struct itree_tree *);
extern intmax_t itree_size (struct itree_tree *);
extern void itree_clear (struct itree_tree *);
extern void itree_insert (struct itree_tree *, struct itree_node *,
			  ptrdiff_t, ptrdiff_t);
extern struct itree_node *itree_remove (struct itree_tree *,
					struct itree_node *);
extern void itree_insert_gap (struct itree_tree *, ptrdiff_t, ptrdiff_t, bool);
extern void itree_delete_gap (struct itree_tree *, ptrdiff_t, ptrdiff_t);

/* Iteration functions.  Almost all code should use ITREE_FOREACH
   instead.  */
extern struct itree_iterator *itree_iterator_start (struct itree_iterator *,
						    struct itree_tree *,
						    ptrdiff_t,
						    ptrdiff_t,
						    enum itree_order);
extern void itree_iterator_narrow (struct itree_iterator *, ptrdiff_t,
				   ptrdiff_t);
extern struct itree_node *itree_iterator_next (struct itree_iterator *);

/* State used when iterating interval. */
struct itree_iterator
  {
    struct itree_node *node;
    ptrdiff_t begin;
    ptrdiff_t end;
    uintmax_t otick;    /* A copy of the tree's `otick`.  */
    enum itree_order order;
  };

/* Iterate over the intervals between BEG and END in the tree T.
   N will hold successive nodes.  ORDER can be one of : `ASCENDING`,
   `DESCENDING`, `POST_ORDER`, or `PRE_ORDER`.
   It should be used as:

      ITREE_FOREACH (n, t, beg, end, order)
        {
          .. do the thing with n ..
        }

   BEWARE:
   - The expression T may be evaluated more than once, so make sure
     it is cheap and pure.
   - Don't modify the tree during the iteration.
 */
#define ITREE_FOREACH(n, t, beg, end, order)                        \
  /* FIXME: We'd want to declare `n` right here, but I can't figure out
     how to make that work here: the `for` syntax only allows a single
     clause for the var declarations where we need 2 different types.
     We could use the `struct {foo x; bar y; } p;` trick to declare two
     vars `p.x` and `p.y` of unrelated types, but then none of the names
     of the vars matches the `n` we receive :-(.  */             \
  if (!t)                                                        \
    { }                                                          \
  else                                                           \
    for (struct itree_iterator itree_local_iter_,                \
                               *itree_iter_                      \
            = itree_iterator_start (&itree_local_iter_,          \
                                    t, beg, end, ITREE_##order); \
          ((n = itree_iterator_next (itree_iter_)));)

#define ITREE_FOREACH_NARROW(beg, end) \
  itree_iterator_narrow (itree_iter_, beg, end)

#endif
