/* Test the interval data-structure in itree.c.

Copyright (c) 2017-2026 Free Software Foundation, Inc.

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

#include <config.h>

#include <stdarg.h>
#include <stdlib.h>

#include <check.h>
#include "emacs-compat.h"

#define EMACS_LISP_H            /* lisp.h inclusion guard */
#define ITREE_TESTING
#include "itree.c"

/* Globals.  */

static struct itree_tree tree;
static struct itree_node A, B, C, D, E;
static struct itree_node N_05, N_10, N_15, N_20, N_30, N_40;
static struct itree_node N_50, N_70, N_80, N_90, N_85, N_95;

/* Basic tests of the itree_tree data-structure.  */

/* +===================================================================================+
 * | Insert
 * +===================================================================================+ */

/* The graphs below display the trees after each insertion (as they
   should be).  See the source code for the different cases
   applied.  */

static void
test_insert1_setup (void)
{
  enum { N = 6 };
  const int values[N] = {50, 30, 20, 10, 15, 5};
  struct itree_node *nodes[N] = {&N_50, &N_30, &N_20, &N_10, &N_15, &N_05};
  itree_init (&tree);
  for (int i = 0; i < N; ++i)
    {
      nodes[i]->begin = nodes[i]->end = values[i];
      nodes[i]->otick = tree.otick;
    }
}

START_TEST (test_insert_1)
{
  /*
   *                 [50]
   */

  itree_insert_node (&tree, &N_50);
  ck_assert (! N_50.red);
  ck_assert_ptr_eq (&N_50, tree.root);
}
END_TEST

START_TEST (test_insert_2)
{
  /*
   *                 [50]
   *                /
   *              (30)
   */

  itree_insert_node (&tree, &N_50);
  itree_insert_node (&tree, &N_30);
  ck_assert (! N_50.red);
  ck_assert (N_30.red);
  ck_assert_ptr_eq (&N_50, tree.root);
  ck_assert_ptr_eq (N_30.parent, &N_50);
  ck_assert_ptr_eq (N_50.left, &N_30);
  ck_assert_ptr_null (N_50.right);
  ck_assert_ptr_null (N_30.left);
  ck_assert_ptr_null (N_30.right);
}
END_TEST

START_TEST (test_insert_3)
{
  /* case 3.a
   *                [30]
   *               /    \
   *             (20)   (50)
   */

  itree_insert_node (&tree, &N_50);
  itree_insert_node (&tree, &N_30);
  itree_insert_node (&tree, &N_20);
  ck_assert (N_50.red);
  ck_assert (! N_30.red);
  ck_assert (N_20.red);
  ck_assert_ptr_eq (&N_30, tree.root);
  ck_assert_ptr_eq (N_50.parent, &N_30);
  ck_assert_ptr_eq (N_30.right, &N_50);
  ck_assert_ptr_eq (N_30.left, &N_20);
  ck_assert_ptr_null (N_20.left);
  ck_assert_ptr_null (N_20.right);
  ck_assert_ptr_eq (N_20.parent, &N_30);
}
END_TEST

START_TEST (test_insert_4)
{
  /* 1.a
   *                [30]
   *               /    \
   *             [20]   [50]
   *             /
   *           (10)
   */

  itree_insert_node (&tree, &N_50);
  itree_insert_node (&tree, &N_30);
  itree_insert_node (&tree, &N_20);
  itree_insert_node (&tree, &N_10);
  ck_assert (! N_50.red);
  ck_assert (! N_30.red);
  ck_assert (! N_20.red);
  ck_assert (N_10.red);
  ck_assert_ptr_eq (&N_30, tree.root);
  ck_assert_ptr_eq (N_50.parent, &N_30);
  ck_assert_ptr_eq (N_30.right, &N_50);
  ck_assert_ptr_eq (N_30.left, &N_20);
  ck_assert_ptr_eq (N_20.left, &N_10);
  ck_assert_ptr_null (N_20.right);
  ck_assert_ptr_eq (N_20.parent, &N_30);
  ck_assert_ptr_eq (N_10.parent, &N_20);
  ck_assert_ptr_eq (N_20.left, &N_10);
  ck_assert_ptr_null (N_10.right);
}
END_TEST

START_TEST (test_insert_5)
{
  /* 2.a
   *                [30]
   *               /    \
   *             [15]   [50]
   *             /  \
   *           (10) (20)
   */

  itree_insert_node (&tree, &N_50);
  itree_insert_node (&tree, &N_30);
  itree_insert_node (&tree, &N_20);
  itree_insert_node (&tree, &N_10);
  itree_insert_node (&tree, &N_15);
  ck_assert (! N_50.red);
  ck_assert (! N_30.red);
  ck_assert (N_20.red);
  ck_assert (N_10.red);
  ck_assert (! N_15.red);
  ck_assert_ptr_eq (&N_30, tree.root);
  ck_assert_ptr_eq (N_50.parent, &N_30);
  ck_assert_ptr_eq (N_30.right, &N_50);
  ck_assert_ptr_eq (N_30.left, &N_15);
  ck_assert_ptr_null (N_20.left);
  ck_assert_ptr_null (N_20.right);
  ck_assert_ptr_eq (N_20.parent, &N_15);
  ck_assert_ptr_eq (N_10.parent, &N_15);
  ck_assert_ptr_null (N_20.left);
  ck_assert_ptr_null (N_10.right);
  ck_assert_ptr_eq (N_15.right, &N_20);
  ck_assert_ptr_eq (N_15.left, &N_10);
  ck_assert_ptr_eq (N_15.parent, &N_30);
}
END_TEST

START_TEST (test_insert_6)
{
  /* 1.a
   *                [30]
   *               /    \
   *             (15)   [50]
   *             /  \
   *           [10] [20]
   *           /
   *         (5)
   */

  itree_insert_node (&tree, &N_50);
  itree_insert_node (&tree, &N_30);
  itree_insert_node (&tree, &N_20);
  itree_insert_node (&tree, &N_10);
  itree_insert_node (&tree, &N_15);
  itree_insert_node (&tree, &N_05);
  ck_assert (! N_50.red);
  ck_assert (! N_30.red);
  ck_assert (! N_20.red);
  ck_assert (! N_10.red);
  ck_assert (N_15.red);
  ck_assert (N_05.red);
  ck_assert_ptr_eq (&N_30, tree.root);
  ck_assert_ptr_eq (N_50.parent, &N_30);
  ck_assert_ptr_eq (N_30.right, &N_50);
  ck_assert_ptr_eq (N_30.left, &N_15);
  ck_assert_ptr_null (N_20.left);
  ck_assert_ptr_null (N_20.right);
  ck_assert_ptr_eq (N_20.parent, &N_15);
  ck_assert_ptr_eq (N_10.parent, &N_15);
  ck_assert_ptr_null (N_20.left);
  ck_assert_ptr_null (N_10.right);
  ck_assert_ptr_eq (N_15.right, &N_20);
  ck_assert_ptr_eq (N_15.left, &N_10);
  ck_assert_ptr_eq (N_15.parent, &N_30);
  ck_assert_ptr_eq (N_05.parent, &N_10);
  ck_assert_ptr_eq (N_10.left, &N_05);
  ck_assert_ptr_null (N_05.right);
}
END_TEST



/* These are the mirror cases to the above ones.  */

static void
test_insert2_setup (void)
{
  enum { N = 6 };
  const int values[] = {50, 70, 80, 90, 85, 95};
  struct itree_node *nodes[N] = {&N_50, &N_70, &N_80, &N_90, &N_85, &N_95};
  itree_init (&tree);
  for (int i = 0; i < N; ++i)
    {
      nodes[i]->begin = nodes[i]->end = values[i];
      nodes[i]->otick = tree.otick;
    }
}

START_TEST (test_insert_7)
{
  /*
   *                 [50]
   */

  itree_insert_node (&tree, &N_50);
  ck_assert (! N_50.red);
  ck_assert_ptr_eq (&N_50, tree.root);
}
END_TEST

START_TEST (test_insert_8)
{
  /*
   *                 [50]
   *                    \
   *                   (70)
   */

  itree_insert_node (&tree, &N_50);
  itree_insert_node (&tree, &N_70);
  ck_assert (! N_50.red);
  ck_assert (N_70.red);
  ck_assert_ptr_eq (&N_50, tree.root);
  ck_assert_ptr_eq (N_70.parent, &N_50);
  ck_assert_ptr_eq (N_50.right, &N_70);
  ck_assert_ptr_null (N_50.left);
  ck_assert_ptr_null (N_70.right);
  ck_assert_ptr_null (N_70.left);
}
END_TEST

START_TEST (test_insert_9)
{
  /* 3.a
   *                [70]
   *               /    \
   *             (50)   (80)
   */

  itree_insert_node (&tree, &N_50);
  itree_insert_node (&tree, &N_70);
  itree_insert_node (&tree, &N_80);
  ck_assert (N_50.red);
  ck_assert (! N_70.red);
  ck_assert (N_80.red);
  ck_assert_ptr_eq (&N_70, tree.root);
  ck_assert_ptr_eq (N_50.parent, &N_70);
  ck_assert_ptr_eq (N_70.right, &N_80);
  ck_assert_ptr_eq (N_70.left, &N_50);
  ck_assert_ptr_null (N_80.right);
  ck_assert_ptr_null (N_80.left);
  ck_assert_ptr_eq (N_80.parent, &N_70);
}
END_TEST

START_TEST (test_insert_10)
{
  /* 1.b
   *                [70]
   *               /    \
   *             [50]   [80]
   *                      \
   *                      (90)
   */

  itree_insert_node (&tree, &N_50);
  itree_insert_node (&tree, &N_70);
  itree_insert_node (&tree, &N_80);
  itree_insert_node (&tree, &N_90);
  ck_assert (! N_50.red);
  ck_assert (! N_70.red);
  ck_assert (! N_80.red);
  ck_assert (N_90.red);
  ck_assert_ptr_eq (&N_70, tree.root);
  ck_assert_ptr_eq (N_50.parent, &N_70);
  ck_assert_ptr_eq (N_70.right, &N_80);
  ck_assert_ptr_eq (N_70.left, &N_50);
  ck_assert_ptr_eq (N_80.right, &N_90);
  ck_assert_ptr_null (N_80.left);
  ck_assert_ptr_eq (N_80.parent, &N_70);
  ck_assert_ptr_eq (N_90.parent, &N_80);
  ck_assert_ptr_eq (N_80.right, &N_90);
  ck_assert_ptr_null (N_90.left);
}
END_TEST

START_TEST (test_insert_11)
{
  /* 2.b
   *                [70]
   *               /    \
   *             [50]   [85]
   *                    /  \
   *                  (80) (90)
   */

  itree_insert_node (&tree, &N_50);
  itree_insert_node (&tree, &N_70);
  itree_insert_node (&tree, &N_80);
  itree_insert_node (&tree, &N_90);
  itree_insert_node (&tree, &N_85);
  ck_assert (! N_50.red);
  ck_assert (! N_70.red);
  ck_assert (N_80.red);
  ck_assert (N_90.red);
  ck_assert (! N_85.red);
  ck_assert_ptr_eq (&N_70, tree.root);
  ck_assert_ptr_eq (N_50.parent, &N_70);
  ck_assert_ptr_eq (N_70.right, &N_85);
  ck_assert_ptr_eq (N_70.left, &N_50);
  ck_assert_ptr_null (N_80.right);
  ck_assert_ptr_null (N_80.left);
  ck_assert_ptr_eq (N_80.parent, &N_85);
  ck_assert_ptr_eq (N_90.parent, &N_85);
  ck_assert_ptr_null (N_80.right);
  ck_assert_ptr_null (N_90.left);
  ck_assert_ptr_eq (N_85.right, &N_90);
  ck_assert_ptr_eq (N_85.left, &N_80);
  ck_assert_ptr_eq (N_85.parent, &N_70);

}
END_TEST

START_TEST (test_insert_12)
{
  /* 1.b
   *                [70]
   *               /    \
   *             [50]   (85)
   *                    /  \
   *                  [80] [90]
   *                         \
   *                        (95)
   */

  itree_insert_node (&tree, &N_50);
  itree_insert_node (&tree, &N_70);
  itree_insert_node (&tree, &N_80);
  itree_insert_node (&tree, &N_90);
  itree_insert_node (&tree, &N_85);
  itree_insert_node (&tree, &N_95);
  ck_assert (! N_50.red);
  ck_assert (! N_70.red);
  ck_assert (! N_80.red);
  ck_assert (! N_90.red);
  ck_assert (N_85.red);
  ck_assert (N_95.red);
  ck_assert_ptr_eq (&N_70, tree.root);
  ck_assert_ptr_eq (N_50.parent, &N_70);
  ck_assert_ptr_eq (N_70.right, &N_85);
  ck_assert_ptr_eq (N_70.left, &N_50);
  ck_assert_ptr_null (N_80.right);
  ck_assert_ptr_null (N_80.left);
  ck_assert_ptr_eq (N_80.parent, &N_85);
  ck_assert_ptr_eq (N_90.parent, &N_85);
  ck_assert_ptr_null (N_80.right);
  ck_assert_ptr_null (N_90.left);
  ck_assert_ptr_eq (N_85.right, &N_90);
  ck_assert_ptr_eq (N_85.left, &N_80);
  ck_assert_ptr_eq (N_85.parent, &N_70);
  ck_assert_ptr_eq (N_95.parent, &N_90);
  ck_assert_ptr_eq (N_90.right, &N_95);
  ck_assert_ptr_null (N_95.left);
}
END_TEST

START_TEST (test_insert_13)
{
  enum { N = 4 };
  const int values[N] = {10, 20, 30, 40};
  struct itree_node *nodes[N] = {&N_10, &N_20, &N_30, &N_40};
  itree_init (&tree);
  for (int i = 0; i < N; ++i)
    itree_insert (&tree, nodes[i], values[i], values[i]);

  ck_assert_ptr_eq (tree.root, &N_20);
  ck_assert_ptr_eq (N_20.left, &N_10);
  ck_assert_ptr_eq (N_20.right, &N_30);
  ck_assert_ptr_eq (N_30.right, &N_40);
  ck_assert (! N_10.red);
  ck_assert (! N_20.red);
  ck_assert (! N_30.red);
  ck_assert (N_40.red);
}
END_TEST

START_TEST (test_insert_14)
{
  enum { N = 3 };
  struct itree_node nodes[N] = {0};
  itree_init (&tree);

  for (int i = 0; i < N; ++i)
    itree_insert (&tree, &nodes[i], 10, 10);
  for (int i = 0; i < N; ++i)
    ck_assert (itree_contains (&tree, &nodes[i]));
}
END_TEST


/* +===================================================================================+
 * | Remove
 * +===================================================================================+ */

/* Creating proper test trees for the formal tests via insertions is
   way too tedious, so we just fake it and only test the
   fix-routine.  */
static void
test_remove1_setup (void)
{
  itree_init (&tree);
  tree.root = &B;
  A.parent = &B; B.parent = NULL; C.parent = &D; D.parent = &B; E.parent = &D;
  A.left = A.right = C.left = C.right = E.left = E.right = NULL;
  B.left = &A; B.right = &D;
  D.left = &C; D.right = &E;
  A.offset = B.offset = C.offset = D.offset = E.offset = 0;
  A.otick = B.otick = C.otick = D.otick = E.otick = tree.otick;
}

/* 1.a -> 2.a
 *                [B]
 *               /    \
 *             [A]    (D)
 *                    /  \
 *                 [C]   [E]
 */

START_TEST (test_remove_1)
{
  B.red = A.red = C.red = E.red = false;
  D.red = true;
  itree_remove_fix (&tree, &A, &B);

  ck_assert (! A.red);
  ck_assert (! B.red);
  ck_assert (C.red);
  ck_assert (! D.red);
  ck_assert (! E.red);
  ck_assert_ptr_eq (A.parent, &B);
  ck_assert_ptr_eq (B.left, &A);
  ck_assert_ptr_eq (B.right, &C);
  ck_assert_ptr_eq (C.parent, &B);
  ck_assert_ptr_eq (E.parent, &D);
  ck_assert_ptr_eq (D.right, &E);
  ck_assert_ptr_eq (D.left, &B);
  ck_assert_ptr_eq (tree.root, &D);
}
END_TEST

/* 2.a */
START_TEST (test_remove_2)
{
  B.red = D.red = A.red = C.red = E.red = false;
  itree_remove_fix (&tree, &A, &B);

  ck_assert (! A.red);
  ck_assert (! B.red);
  ck_assert (! C.red);
  ck_assert (D.red);
  ck_assert (! E.red);
  ck_assert_ptr_eq (A.parent, &B);
  ck_assert_ptr_eq (B.left, &A);
  ck_assert_ptr_eq (B.right, &D);
  ck_assert_ptr_eq (C.parent, &D);
  ck_assert_ptr_eq (E.parent, &D);
  ck_assert_ptr_eq (tree.root, &B);
}
END_TEST

/* 3.a -> 4.a */
START_TEST (test_remove_3)
{
  D.red = A.red = E.red = false;
  B.red = C.red = true;
  itree_remove_fix (&tree, &A, &B);

  ck_assert (! A.red);
  ck_assert (! B.red);
  ck_assert (! C.red);
  ck_assert (! D.red);
  ck_assert (! E.red);
  ck_assert_ptr_eq (A.parent, &B);
  ck_assert_ptr_eq (B.left, &A);
  ck_assert_ptr_null (B.right);
  ck_assert_ptr_eq (&C, tree.root);
  ck_assert_ptr_eq (C.left, &B);
  ck_assert_ptr_eq (C.right, &D);
  ck_assert_ptr_eq (E.parent, &D);
  ck_assert_ptr_null (D.left);
}
END_TEST

/* 4.a */
START_TEST (test_remove_4)
{
  B.red = C.red = E.red = true;
  A.red = D.red = false;
  itree_remove_fix (&tree, &A, &B);

  ck_assert (! A.red);
  ck_assert (! B.red);
  ck_assert (C.red);
  ck_assert (! D.red);
  ck_assert (! E.red);
  ck_assert_ptr_eq (A.parent, &B);
  ck_assert_ptr_eq (B.left, &A);
  ck_assert_ptr_eq (B.right, &C);
  ck_assert_ptr_eq (C.parent, &B);
  ck_assert_ptr_eq (E.parent, &D);
  ck_assert_ptr_eq (tree.root, &D);
}
END_TEST



/* These are the mirrored cases.  */

static void
test_remove2_setup (void)
{
  itree_init (&tree);
  tree.root = &B;
  A.parent = &B; B.parent = NULL; C.parent = &D; D.parent = &B; E.parent = &D;
  A.right = A.left = C.right = C.left = E.right = E.left = NULL;
  B.right = &A; B.left = &D;
  D.right = &C; D.left = &E;
}

/* 1.b -> 2.b
 *                [B]
 *               /    \
 *             [A]    (D)
 *                    /  \
 *                 [C]   [E]
 */

START_TEST (test_remove_5)
{
  B.red = A.red = C.red = E.red = false;
  D.red = true;
  itree_remove_fix (&tree, &A, &B);

  ck_assert (! A.red);
  ck_assert (! B.red);
  ck_assert (C.red);
  ck_assert (! D.red);
  ck_assert (! E.red);
  ck_assert_ptr_eq (A.parent, &B);
  ck_assert_ptr_eq (B.right, &A);
  ck_assert_ptr_eq (B.left, &C);
  ck_assert_ptr_eq (C.parent, &B);
  ck_assert_ptr_eq (E.parent, &D);
  ck_assert_ptr_eq (D.left, &E);
  ck_assert_ptr_eq (D.right, &B);
  ck_assert_ptr_eq (tree.root, &D);
}
END_TEST

/* 2.b */
START_TEST (test_remove_6)
{
  B.red = D.red = A.red = C.red = E.red = false;
  itree_remove_fix (&tree, &A, &B);

  ck_assert (! A.red);
  ck_assert (! B.red);
  ck_assert (! C.red);
  ck_assert (D.red);
  ck_assert (! E.red);
  ck_assert_ptr_eq (A.parent, &B);
  ck_assert_ptr_eq (B.right, &A);
  ck_assert_ptr_eq (B.left, &D);
  ck_assert_ptr_eq (C.parent, &D);
  ck_assert_ptr_eq (E.parent, &D);
  ck_assert_ptr_eq (tree.root, &B);
}
END_TEST

/* 3.b -> 4.b */
START_TEST (test_remove_7)
{
  D.red = A.red = E.red = false;
  B.red = C.red = true;
  itree_remove_fix (&tree, &A, &B);

  ck_assert (! A.red);
  ck_assert (! B.red);
  ck_assert (! C.red);
  ck_assert (! D.red);
  ck_assert (! E.red);
  ck_assert_ptr_eq (A.parent, &B);
  ck_assert_ptr_eq (B.right, &A);
  ck_assert_ptr_null (B.left);
  ck_assert_ptr_eq (&C, tree.root);
  ck_assert_ptr_eq (C.right, &B);
  ck_assert_ptr_eq (C.left, &D);
  ck_assert_ptr_eq (E.parent, &D);
  ck_assert_ptr_null (D.right);
}
END_TEST

/* 4.b */
START_TEST (test_remove_8)
{
  B.red = C.red = E.red = true;
  A.red = D.red = false;
  itree_remove_fix (&tree, &A, &B);

  ck_assert (! A.red);
  ck_assert (! B.red);
  ck_assert (C.red);
  ck_assert (! D.red);
  ck_assert (! E.red);
  ck_assert_ptr_eq (A.parent, &B);
  ck_assert_ptr_eq (B.right, &A);
  ck_assert_ptr_eq (B.left, &C);
  ck_assert_ptr_eq (C.parent, &B);
  ck_assert_ptr_eq (E.parent, &D);
  ck_assert_ptr_eq (tree.root, &D);
}
END_TEST

START_TEST (test_remove_9)
{
  enum { N = 4 };
  const int values[N] = {10, 20, 30, 40};
  struct itree_node *nodes[N] = {&N_10, &N_20, &N_30, &N_40};
  itree_init (&tree);
  for (int i = 0; i < N; ++i)
    itree_insert (&tree, nodes[i], values[i], values[i]);

  ck_assert (tree.root == &N_20);
  ck_assert (N_20.left == &N_10);
  ck_assert (N_20.right == &N_30);
  ck_assert (N_30.right == &N_40);
  ck_assert (! N_20.red);
  ck_assert (! N_10.red);
  ck_assert (! N_30.red);
  ck_assert (N_40.red);

  itree_remove (&tree, &N_10);

  ck_assert_ptr_eq (tree.root, &N_30);
  ck_assert_ptr_null (N_30.parent);
  ck_assert_ptr_eq (N_30.left, &N_20);
  ck_assert_ptr_eq (N_30.right, &N_40);
  ck_assert (! N_20.red);
  ck_assert (! N_30.red);
  ck_assert (! N_40.red);
}
END_TEST

static void
shuffle (int *index, int n)
{
  for (int i = n - 1; i >= 0; --i)
    {
      int j = random () % (i + 1);
      int h = index[j];
      index[j] = index[i];
      index[i] = h;
    }
}

START_TEST (test_remove_10)
{
  enum { N = 3 };
  int index[N];
  for (int i = 0; i < N; ++i)
    index[i] = i;
  srand (42);
  shuffle (index, N);

  itree_init (&tree);
  struct itree_node nodes[N] = {0};
  for (int i = 0; i < N; ++i)
    {
      ptrdiff_t pos = (i + 1) * 10;
      itree_insert (&tree, &nodes[index[i]], pos, pos + 1);
    }

  shuffle (index, N);
  for (int i = 0; i < N; ++i)
    {
      ck_assert (itree_contains (&tree, &nodes[index[i]]));
      itree_remove (&tree, &nodes[index[i]]);
    }
  ck_assert (itree_empty_p (&tree));
  ck_assert_int_eq (tree.size, 0);
}
END_TEST


/* +===================================================================================+
 * | Generator
 * +===================================================================================+ */

START_TEST (test_generator_1)
{
  struct itree_node node = {0}, *n;
  struct itree_iterator it, *g;
  itree_init (&tree);

  itree_insert (&tree, &node, 10, 20);
  g = itree_iterator_start (&it, &tree, 0, 30, ITREE_ASCENDING);
  n = itree_iterator_next (g);
  ck_assert_ptr_eq (n, &node);
  ck_assert_int_eq (n->begin, 10);
  ck_assert_int_eq (n->end, 20);
  ck_assert_ptr_null (itree_iterator_next (g));
  ck_assert_ptr_null (itree_iterator_next (g));
  ck_assert_ptr_null (itree_iterator_next (g));

  g = itree_iterator_start (&it, &tree, 30, 50, ITREE_ASCENDING);
  ck_assert_ptr_null (itree_iterator_next (g));
  ck_assert_ptr_null (itree_iterator_next (g));
  ck_assert_ptr_null (itree_iterator_next (g));
}
END_TEST

static void
test_check_generator (struct itree_tree *tree,
                      ptrdiff_t begin, ptrdiff_t end,
                      int n, ...)
{
  va_list ap;
  struct itree_iterator it, *g =
    itree_iterator_start (&it, tree, begin, end, ITREE_ASCENDING);

  va_start (ap, n);
  for (int i = 0; i < n; ++i)
    {
      struct itree_node *node = itree_iterator_next (g);
      ck_assert_ptr_nonnull (node);
      ck_assert_int_eq (node->begin, va_arg (ap, ptrdiff_t));
    }
  va_end (ap);
  ck_assert_ptr_null (itree_iterator_next (g));
  ck_assert_ptr_null (itree_iterator_next (g));
}

START_TEST (test_generator_2)
{
  itree_init (&tree);
  struct itree_node nodes[3] = {0};
  for (int i = 0; i < 3; ++i)
    itree_insert (&tree, &nodes[i], 10 * (i + 1), 10 * (i + 2));

  test_check_generator (&tree, 0, 50, 3,
                        10, 20, 30);
  test_check_generator (&tree, 0, 10, 0);
  test_check_generator (&tree, 40, 50, 0);
  test_check_generator (&tree, 15, 35, 3,
                        10, 20, 30);
  test_check_generator (&tree, -100, -50, 0);
  test_check_generator (&tree, -100, -50, 0);
  test_check_generator (&tree, 100, 50, 0);
  test_check_generator (&tree, 100, 150, 0);
  test_check_generator (&tree, 0, 0, 0);
  test_check_generator (&tree, 40, 40, 0);
  test_check_generator (&tree, 30, 30, 0);
  test_check_generator (&tree, 35, 35, 1,
                        30);
}
END_TEST

static void
test_create_tree (struct itree_node *nodes, int n, bool doshuffle)
{
  int *index = calloc (n, sizeof (int));
  for (int i = 0; i < n; ++i)
    index[i] = i;
  if (doshuffle)
    {
      srand (42);
      shuffle (index, n);
    }

  itree_init (&tree);
  for (int i = 0; i < n; ++i)
    {
      struct itree_node *node = &nodes[index[i]];
      itree_insert (&tree, node, node->begin, node->end);
    }
  free (index);
}

START_TEST (test_generator_3)
{
  enum { N = 3 };
  struct itree_node nodes[N] = {{.begin = 10, .end = 10},
                                {.begin = 10, .end = 10},
                                {.begin = 10, .end = 10}};
  test_create_tree (nodes, N, true);
  test_check_generator (&tree, 0, 10, 0);
  test_check_generator (&tree, 10, 10, 3,
                        10, 10, 10);
  test_check_generator (&tree, 10, 20, 3,
                        10, 10, 10);
}
END_TEST

START_TEST (test_generator_5)
{
  enum { N = 4 };
  struct itree_node nodes[N] = {{.begin = 10, .end = 30},
                                {.begin = 20, .end = 40},
                                {.begin = 30, .end = 50},
                                {.begin = 40, .end = 60}};
  test_create_tree (nodes, N, false);
  struct itree_iterator it, *g =
    itree_iterator_start (&it, &tree, 0, 100, ITREE_PRE_ORDER);
  for (int i = 0; i < N; ++i)
    {
      struct itree_node *n = itree_iterator_next (g);
      ck_assert_ptr_nonnull (n);
      switch (i)
        {
        case 0: ck_assert_int_eq (20, n->begin); break;
        case 1: ck_assert_int_eq (10, n->begin); break;
        case 2: ck_assert_int_eq (30, n->begin); break;
        case 3: ck_assert_int_eq (40, n->begin); break;
        }
    }
}
END_TEST

START_TEST (test_generator_6)
{
  enum { N = 4 };
  struct itree_node nodes[N] = {{.begin = 10, .end = 30},
                                {.begin = 20, .end = 40},
                                {.begin = 30, .end = 50},
                                {.begin = 40, .end = 60}};
  test_create_tree (nodes, N, true);
  struct itree_iterator it, *g =
    itree_iterator_start (&it, &tree, 0, 100, ITREE_ASCENDING);
  for (int i = 0; i < N; ++i)
    {
      struct itree_node *n = itree_iterator_next (g);
      ck_assert_ptr_nonnull (n);
      switch (i)
        {
        case 0: ck_assert_int_eq (10, n->begin); break;
        case 1: ck_assert_int_eq (20, n->begin); break;
        case 2: ck_assert_int_eq (30, n->begin); break;
        case 3: ck_assert_int_eq (40, n->begin); break;
        }
    }
}
END_TEST

START_TEST (test_generator_7)
{
  enum { N = 4 };
  struct itree_node nodes[N] = {{.begin = 10, .end = 30},
                                {.begin = 20, .end = 40},
                                {.begin = 30, .end = 50},
                                {.begin = 40, .end = 60}};
  test_create_tree (nodes, N, true);
  struct itree_iterator it, *g =
    itree_iterator_start (&it, &tree, 0, 100, ITREE_DESCENDING);
  for (int i = 0; i < N; ++i)
    {
      struct itree_node *n = itree_iterator_next (g);
      ck_assert_ptr_nonnull (n);
      switch (i)
        {
        case 0: ck_assert_int_eq (40, n->begin); break;
        case 1: ck_assert_int_eq (30, n->begin); break;
        case 2: ck_assert_int_eq (20, n->begin); break;
        case 3: ck_assert_int_eq (10, n->begin); break;
        }
    }
}
END_TEST

START_TEST (test_generator_8)
{
  enum { N = 2 };
  struct itree_node nodes[N] = {{.begin = 20, .end = 30},
                                {.begin = 40, .end = 50}};
  test_create_tree (nodes, N, false);
  struct itree_iterator it, *g =
    itree_iterator_start (&it, &tree, 1, 60, ITREE_DESCENDING);
  struct itree_node *n = itree_iterator_next (g);
  ck_assert_int_eq (n->begin, 40);
  itree_iterator_narrow (g, 50, 60);
  n = itree_iterator_next (g);
  ck_assert_ptr_null (n);
}
END_TEST

START_TEST (test_generator_9)
{
  enum { N = 2 };
  struct itree_node nodes[N] = {{.begin = 25, .end = 25},
                                {.begin = 20, .end = 30}};
  test_create_tree (nodes, N, false);
  struct itree_iterator it, *g =
    itree_iterator_start (&it, &tree, 1, 30, ITREE_DESCENDING);
  struct itree_node *n = itree_iterator_next (g);
  ck_assert_int_eq (n->begin, 25);
  itree_iterator_narrow (g, 25, 30);
  n = itree_iterator_next (g);
  ck_assert_int_eq (n->begin, 20);
}
END_TEST


/* +===================================================================================+
 * | Insert Gap
 * +===================================================================================+ */

static struct itree_tree gap_tree;
static struct itree_node gap_node;

#define N_BEG (itree_node_begin (&gap_tree, &gap_node))
#define N_END (itree_node_end (&gap_tree, &gap_node))

static void
test_setup_gap_node (ptrdiff_t begin, ptrdiff_t end,
                     bool front_advance, bool rear_advance)
{
  itree_init (&gap_tree);
  gap_node.front_advance = front_advance;
  gap_node.rear_advance = rear_advance;
  itree_insert (&gap_tree, &gap_node, begin, end);
}

static void
test_setup_gap_node_noadvance (ptrdiff_t begin, ptrdiff_t end)
{
  test_setup_gap_node (begin, end, false, false);
}

START_TEST (test_gap_insert_1)
{
  test_setup_gap_node_noadvance (100, 200);
  itree_insert_gap (&gap_tree, 100 + 10, 20, false);
  ck_assert_int_eq (N_BEG, 100);
  ck_assert_int_eq (N_END, 200 + 20);
}
END_TEST

START_TEST (test_gap_insert_2)
{
  test_setup_gap_node_noadvance (100, 200);
  itree_insert_gap (&gap_tree, 300, 10, false);
  ck_assert_int_eq (N_BEG, 100);
  ck_assert_int_eq (N_END, 200);
}
END_TEST

START_TEST (test_gap_insert_3)
{
  test_setup_gap_node_noadvance (100, 200);
  itree_insert_gap (&gap_tree, 0, 15, false);
  ck_assert_int_eq (N_BEG, 100 + 15);
  ck_assert_int_eq (N_END, 200 + 15);
}
END_TEST

START_TEST (test_gap_insert_4)
{
  test_setup_gap_node (100, 200, true, false);
  itree_insert_gap (&gap_tree, 100, 20, false);
  ck_assert_int_eq (N_BEG, 100 + 20);
  ck_assert_int_eq (N_END, 200 + 20);

}
END_TEST

START_TEST (test_gap_insert_5)
{
  test_setup_gap_node_noadvance (100, 200);
  itree_insert_gap (&gap_tree, 100, 20, false);
  ck_assert_int_eq (N_BEG, 100);
  ck_assert_int_eq (N_END, 200 + 20);

}
END_TEST

START_TEST (test_gap_insert_6)
{
  test_setup_gap_node (100, 200, false, true);
  itree_insert_gap (&gap_tree, 200, 20, false);
  ck_assert_int_eq (N_BEG, 100);
  ck_assert_int_eq (N_END, 200 + 20);

}
END_TEST

START_TEST (test_gap_insert_7)
{
  test_setup_gap_node_noadvance (100, 200);
  itree_insert_gap (&gap_tree, 200, 20, false);
  ck_assert_int_eq (N_BEG, 100);
  ck_assert_int_eq (N_END, 200);

}
END_TEST

START_TEST (test_gap_insert_8)
{
  test_setup_gap_node (100, 100, true, true);
  itree_insert_gap (&gap_tree, 100, 20, false);
  ck_assert_int_eq (N_BEG, 100 + 20);
  ck_assert_int_eq (N_END, 100 + 20);

}
END_TEST

START_TEST (test_gap_insert_9)
{
  test_setup_gap_node (100, 100, false, true);
  itree_insert_gap (&gap_tree, 100, 20, false);
  ck_assert_int_eq (N_BEG, 100);
  ck_assert_int_eq (N_END, 100 + 20);

}
END_TEST

START_TEST (test_gap_insert_10)
{
  test_setup_gap_node (100, 100, true, false);
  itree_insert_gap (&gap_tree, 100, 20, false);
  ck_assert_int_eq (N_BEG, 100);
  ck_assert_int_eq (N_END, 100);

}
END_TEST

START_TEST (test_gap_insert_11)
{
  test_setup_gap_node_noadvance (100, 100);
  itree_insert_gap (&gap_tree, 100, 20, false);
  ck_assert_int_eq (N_BEG, 100);
  ck_assert_int_eq (N_END, 100);

}
END_TEST


/* +===================================================================================+
 * | Delete Gap
 * +===================================================================================+ */

START_TEST (test_gap_delete_1)
{
  test_setup_gap_node_noadvance (100, 200);
  itree_delete_gap (&gap_tree, 100 + 10, 20);
  ck_assert_int_eq (N_BEG, 100);
  ck_assert_int_eq (N_END, 200 - 20);

}
END_TEST

START_TEST (test_gap_delete_2)
{
  test_setup_gap_node_noadvance (100, 200);
  itree_delete_gap (&gap_tree, 200 + 10, 20);
  ck_assert_int_eq (N_BEG, 100);
  ck_assert_int_eq (N_END, 200);

}
END_TEST

START_TEST (test_gap_delete_3)
{
  test_setup_gap_node_noadvance (100, 200);
  itree_delete_gap (&gap_tree, 200, 20);
  ck_assert_int_eq (N_BEG, 100);
  ck_assert_int_eq (N_END, 200);

}
END_TEST

START_TEST (test_gap_delete_4)
{
  test_setup_gap_node_noadvance (100, 200);
  itree_delete_gap (&gap_tree, 100 - 20, 20);
  ck_assert_int_eq (N_BEG, 100 - 20);
  ck_assert_int_eq (N_END, 200 - 20);

}
END_TEST

START_TEST (test_gap_delete_5)
{
  test_setup_gap_node_noadvance (100, 200);
  itree_delete_gap (&gap_tree, 70, 20);
  ck_assert_int_eq (N_BEG, 100 - 20);
  ck_assert_int_eq (N_END, 200 - 20);

}
END_TEST

START_TEST (test_gap_delete_6)
{
  test_setup_gap_node_noadvance (100, 200);
  itree_delete_gap (&gap_tree, 80, 100);
  ck_assert_int_eq (N_BEG, 80);
  ck_assert_int_eq (N_END, 100);
}
END_TEST

START_TEST (test_gap_delete_7)
{
  test_setup_gap_node_noadvance (100, 200);
  itree_delete_gap (&gap_tree, 120, 100);
  ck_assert_int_eq (N_BEG, 100);
  ck_assert_int_eq (N_END, 120);
}
END_TEST

START_TEST (test_gap_delete_8)
{
  test_setup_gap_node_noadvance (100, 200);
  itree_delete_gap (&gap_tree, 100 - 20, 200 + 20);
  ck_assert_int_eq (N_BEG, 100 - 20);
  ck_assert_int_eq (N_END, 100 - 20);

}
END_TEST



static Suite *
basic_suite ()
{
  Suite *s = suite_create ("basic");

  TCase *tc = tcase_create ("insert1");
  tcase_add_checked_fixture (tc, test_insert1_setup, NULL);
  tcase_add_test (tc, test_insert_1);
  tcase_add_test (tc, test_insert_2);
  tcase_add_test (tc, test_insert_3);
  tcase_add_test (tc, test_insert_4);
  tcase_add_test (tc, test_insert_5);
  tcase_add_test (tc, test_insert_6);
  suite_add_tcase (s, tc);

  tc = tcase_create ("insert2");
  tcase_add_checked_fixture (tc, test_insert2_setup, NULL);
  tcase_add_test (tc, test_insert_7);
  tcase_add_test (tc, test_insert_8);
  tcase_add_test (tc, test_insert_9);
  tcase_add_test (tc, test_insert_10);
  tcase_add_test (tc, test_insert_11);
  tcase_add_test (tc, test_insert_12);
  suite_add_tcase (s, tc);

  tc = tcase_create ("insert3");
  tcase_add_test (tc, test_insert_13);
  tcase_add_test (tc, test_insert_14);
  suite_add_tcase (s, tc);

  tc = tcase_create ("remove1");
  tcase_add_checked_fixture (tc, test_remove1_setup, NULL);
  tcase_add_test (tc, test_remove_1);
  tcase_add_test (tc, test_remove_2);
  tcase_add_test (tc, test_remove_3);
  tcase_add_test (tc, test_remove_4);
  suite_add_tcase (s, tc);

  tc = tcase_create ("remove2");
  tcase_add_checked_fixture (tc, test_remove2_setup, NULL);
  tcase_add_test (tc, test_remove_5);
  tcase_add_test (tc, test_remove_6);
  tcase_add_test (tc, test_remove_7);
  tcase_add_test (tc, test_remove_8);
  suite_add_tcase (s, tc);

  tc = tcase_create ("remove3");
  tcase_add_test (tc, test_remove_9);
  tcase_add_test (tc, test_remove_10);
  suite_add_tcase (s, tc);

  tc = tcase_create ("generator");
  tcase_add_test (tc, test_generator_1);
  tcase_add_test (tc, test_generator_2);
  tcase_add_test (tc, test_generator_3);
  tcase_add_test (tc, test_generator_5);
  tcase_add_test (tc, test_generator_6);
  tcase_add_test (tc, test_generator_7);
  tcase_add_test (tc, test_generator_8);
  tcase_add_test (tc, test_generator_9);
  suite_add_tcase (s, tc);

  tc = tcase_create ("insert_gap");
  tcase_add_test (tc, test_gap_insert_1);
  tcase_add_test (tc, test_gap_insert_2);
  tcase_add_test (tc, test_gap_insert_3);
  tcase_add_test (tc, test_gap_insert_4);
  tcase_add_test (tc, test_gap_insert_5);
  tcase_add_test (tc, test_gap_insert_6);
  tcase_add_test (tc, test_gap_insert_7);
  tcase_add_test (tc, test_gap_insert_8);
  tcase_add_test (tc, test_gap_insert_9);
  tcase_add_test (tc, test_gap_insert_10);
  tcase_add_test (tc, test_gap_insert_11);
  suite_add_tcase (s, tc);

  tc = tcase_create ("delete_gap");
  tcase_add_test (tc, test_gap_delete_1);
  tcase_add_test (tc, test_gap_delete_2);
  tcase_add_test (tc, test_gap_delete_3);
  tcase_add_test (tc, test_gap_delete_4);
  tcase_add_test (tc, test_gap_delete_5);
  tcase_add_test (tc, test_gap_delete_6);
  tcase_add_test (tc, test_gap_delete_7);
  tcase_add_test (tc, test_gap_delete_8);
  suite_add_tcase (s, tc);

  return s;
}

int
main (void)
{
  Suite *s = basic_suite ();
  SRunner *sr = srunner_create (s);

  srunner_run_all (sr, CK_ENV);
  int failed = srunner_ntests_failed (sr);
  srunner_free (sr);
  return failed ? EXIT_FAILURE : EXIT_SUCCESS;
}
