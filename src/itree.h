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

#ifndef ITREE_H
#define ITREE_H
#include <config.h>
#include <stddef.h>
#include <inttypes.h>

#include "lisp.h"

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
  bool_bf red : 1;
  bool_bf rear_advance : 1;     /* Same as for marker and overlays.  */
  bool_bf front_advance : 1;    /* Same as for marker and overlays.  */
};

/* The sentinel node, the null node.  */
extern struct interval_node itree_null;
#define ITREE_NULL (&itree_null)

struct interval_tree
{
  struct interval_node *root;
  uintmax_t otick;              /* offset tick, compared with node's otick. */
  intmax_t size;                /* Number of nodes in the tree. */
};

enum interval_tree_order {
  ITREE_ASCENDING,
  ITREE_DESCENDING,
  ITREE_PRE_ORDER,
};

void interval_node_init (struct interval_node *, ptrdiff_t, ptrdiff_t, bool, bool, Lisp_Object);
ptrdiff_t interval_node_begin (struct interval_tree *, struct interval_node *);
ptrdiff_t interval_node_end (struct interval_tree *, struct interval_node *);
void interval_node_set_region (struct interval_tree *, struct interval_node *, ptrdiff_t, ptrdiff_t);
struct interval_tree *interval_tree_create (void);
void interval_tree_destroy (struct interval_tree *);
intmax_t interval_tree_size (struct interval_tree *);
void interval_tree_clear (struct interval_tree *);
void interval_tree_insert (struct interval_tree *, struct interval_node *);
struct interval_node *interval_tree_remove (struct interval_tree *, struct interval_node *);
struct interval_generator *interval_tree_iter_start (struct interval_tree *, ptrdiff_t, ptrdiff_t, enum interval_tree_order,
			       const char* file, int line);
void interval_generator_narrow (struct interval_generator *, ptrdiff_t, ptrdiff_t);
void interval_tree_iter_finish (struct interval_generator *);
struct interval_node *interval_generator_next (struct interval_generator *);
void interval_tree_insert_gap (struct interval_tree *, ptrdiff_t, ptrdiff_t);
void interval_tree_delete_gap (struct interval_tree *, ptrdiff_t, ptrdiff_t);

/* Iterate over the intervals between BEG and END in the tree T.
   N will hold successive nodes.  ORDER can be one of : `ASCENDING`,
   `DESCENDING`, or `PRE_ORDER`.
   It should be used as:

      ITREE_FOREACH (n, t, beg, end, order)
        {
          .. do the thing with n ..
        }

   BEWARE:
   - The expression T may be evaluated more than once, so make sure
     it is cheap a pure.
   - Only a single iteration can happen at a time, so make sure none of the
     code within the loop can start another tree iteration, i.e. it shouldn't
     be able to run ELisp code (or GC for that matter).
   - If you need to exit the loop early, you *have* to call `ITREE_ABORT`
     just before exiting (e.g. with `break` or `return`).
   - Non-local exits are not supported within the body of the loop,
     unless the caller makes sure `ITREE_ABORT` is called via some
     kind of unwind_protect.
   - Don't modify the tree during the iteration.
 */
#define ITREE_FOREACH(n, t, beg, end, order)                        \
  /* FIXME: We'd want to declare `x` right here, but I can't figure out
     how to make that work here: the `for` syntax only allows a single
     clause for the var declarations where we need 2 different types.
     We could use the `struct {foo x; bar y; } p;` trick to declare two
     vars `p.x` and `p.y` of unrelated types, but then none of the names
     of the vars matches the `n` we receive :-(.  */                \
  if (!t)                                                           \
    { }                                                             \
  else                                                              \
    for (struct interval_generator *itree_iter_                     \
            = interval_tree_iter_start (t, beg, end, ITREE_##order, \
                                        __FILE__, __LINE__);        \
          ((n = interval_generator_next (itree_iter_))              \
           || (interval_tree_iter_finish (itree_iter_), false));)

#define ITREE_FOREACH_ABORT() \
  interval_tree_iter_finish (itree_iter_)

#define ITREE_FOREACH_NARROW(beg, end) \
  interval_generator_narrow (itree_iter_, beg, end)

#endif
