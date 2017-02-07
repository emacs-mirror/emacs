#include <config.h>
#include <check.h>
#include <stdlib.h>
#include <stdarg.h>
#include "emacs-compat.h"

#define EMACS_LISP_H            /* lisp.h inclusion guard */
#define ITREE_DEBUG 1
#define ITREE_TESTING
#include "itree.c"

/* Basic tests of the interval_tree data-structure. */

/* +===================================================================================+
 * | Insert
 * +===================================================================================+ */

/* The graphs below display the trees after each insertion (as they
   should be).  See the source code for the different cases
   applied. */

#define N_50 (n[0])
#define N_30 (n[1])
#define N_20 (n[2])
#define N_10 (n[3])
#define N_15 (n[4])
#define N_05 (n[5])

#define DEF_TEST_SETUP()                        \
  struct interval_tree tree;                    \
  struct interval_node n[6];                    \
  interval_tree_init (&tree);                   \
  const int values[] = {50, 30, 20, 10, 15, 5}; \
  for (int i = 0; i < 6; ++i)                   \
    {                                           \
      n[i].begin = values[i];                   \
      n[i].end = values[i];                     \
    }

START_TEST (test_insert_1)
{
  /*
   *                 [50]
   */

  DEF_TEST_SETUP ();
  interval_tree_insert (&tree, &N_50);
  ck_assert (N_50.color == ITREE_BLACK);
  ck_assert (&N_50 == tree.root);
}
END_TEST

START_TEST (test_insert_2)
{
  /*
   *                 [50]
   *                /
   *              (30)
   */

  DEF_TEST_SETUP ();
  interval_tree_insert (&tree, &N_50);
  interval_tree_insert (&tree, &N_30);
  ck_assert (N_50.color == ITREE_BLACK);
  ck_assert (N_30.color == ITREE_RED);
  ck_assert (&N_50 == tree.root);
  ck_assert (N_30.parent == &N_50);
  ck_assert (N_50.left == &N_30);
  ck_assert (N_50.right == &tree.nil);
  ck_assert (N_30.left == &tree.nil);
  ck_assert (N_30.right == &tree.nil);
}
END_TEST

START_TEST (test_insert_3)
{
  /* case 3.a
   *                [30]
   *               /    \
   *             (20)   (50)
   */

  DEF_TEST_SETUP ();
  interval_tree_insert (&tree, &N_50);
  interval_tree_insert (&tree, &N_30);
  interval_tree_insert (&tree, &N_20);
  ck_assert (N_50.color == ITREE_RED);
  ck_assert (N_30.color == ITREE_BLACK);
  ck_assert (N_20.color == ITREE_RED);
  ck_assert (&N_30 == tree.root);
  ck_assert (N_50.parent == &N_30);
  ck_assert (N_30.right == &N_50);
  ck_assert (N_30.left == &N_20);
  ck_assert (N_20.left == &tree.nil);
  ck_assert (N_20.right == &tree.nil);
  ck_assert (N_20.parent == &N_30);
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

  DEF_TEST_SETUP ();
  interval_tree_insert (&tree, &N_50);
  interval_tree_insert (&tree, &N_30);
  interval_tree_insert (&tree, &N_20);
  interval_tree_insert (&tree, &N_10);
  ck_assert (N_50.color == ITREE_BLACK);
  ck_assert (N_30.color == ITREE_BLACK);
  ck_assert (N_20.color == ITREE_BLACK);
  ck_assert (N_10.color == ITREE_RED);
  ck_assert (&N_30 == tree.root);
  ck_assert (N_50.parent == &N_30);
  ck_assert (N_30.right == &N_50);
  ck_assert (N_30.left == &N_20);
  ck_assert (N_20.left == &N_10);
  ck_assert (N_20.right == &tree.nil);
  ck_assert (N_20.parent == &N_30);
  ck_assert (N_10.parent == &N_20);
  ck_assert (N_20.left == &N_10);
  ck_assert (N_10.right == &tree.nil);
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

  DEF_TEST_SETUP ();
  interval_tree_insert (&tree, &N_50);
  interval_tree_insert (&tree, &N_30);
  interval_tree_insert (&tree, &N_20);
  interval_tree_insert (&tree, &N_10);
  interval_tree_insert (&tree, &N_15);
  ck_assert (N_50.color == ITREE_BLACK);
  ck_assert (N_30.color == ITREE_BLACK);
  ck_assert (N_20.color == ITREE_RED);
  ck_assert (N_10.color == ITREE_RED);
  ck_assert (N_15.color == ITREE_BLACK);
  ck_assert (&N_30 == tree.root);
  ck_assert (N_50.parent == &N_30);
  ck_assert (N_30.right == &N_50);
  ck_assert (N_30.left == &N_15);
  ck_assert (N_20.left == &tree.nil);
  ck_assert (N_20.right == &tree.nil);
  ck_assert (N_20.parent == &N_15);
  ck_assert (N_10.parent == &N_15);
  ck_assert (N_20.left == &tree.nil);
  ck_assert (N_10.right == &tree.nil);
  ck_assert (N_15.right == &N_20);
  ck_assert (N_15.left == &N_10);
  ck_assert (N_15.parent == &N_30);

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

  DEF_TEST_SETUP ();
  interval_tree_insert (&tree, &N_50);
  interval_tree_insert (&tree, &N_30);
  interval_tree_insert (&tree, &N_20);
  interval_tree_insert (&tree, &N_10);
  interval_tree_insert (&tree, &N_15);
  interval_tree_insert (&tree, &N_05);
  ck_assert (N_50.color == ITREE_BLACK);
  ck_assert (N_30.color == ITREE_BLACK);
  ck_assert (N_20.color == ITREE_BLACK);
  ck_assert (N_10.color == ITREE_BLACK);
  ck_assert (N_15.color == ITREE_RED);
  ck_assert (N_05.color == ITREE_RED);
  ck_assert (&N_30 == tree.root);
  ck_assert (N_50.parent == &N_30);
  ck_assert (N_30.right == &N_50);
  ck_assert (N_30.left == &N_15);
  ck_assert (N_20.left == &tree.nil);
  ck_assert (N_20.right == &tree.nil);
  ck_assert (N_20.parent == &N_15);
  ck_assert (N_10.parent == &N_15);
  ck_assert (N_20.left == &tree.nil);
  ck_assert (N_10.right == &tree.nil);
  ck_assert (N_15.right == &N_20);
  ck_assert (N_15.left == &N_10);
  ck_assert (N_15.parent == &N_30);
  ck_assert (N_05.parent == &N_10);
  ck_assert (N_10.left == &N_05);
  ck_assert (N_05.right == &tree.nil);
}
END_TEST

#undef N_50
#undef N_30
#undef N_20
#undef N_10
#undef N_15
#undef N_05
#undef DEF_TEST_SETUP



/* These are the mirror cases to the above ones.  */

#define N_50 (n[0])
#define N_70 (n[1])
#define N_80 (n[2])
#define N_90 (n[3])
#define N_85 (n[4])
#define N_95 (n[5])

#define DEF_TEST_SETUP()                                \
  struct interval_tree tree;                            \
  struct interval_node n[6];                            \
  interval_tree_init (&tree);                           \
  const int values[] = {50, 70, 80, 90, 85, 95};        \
  for (int i = 0; i < 6; ++i)                           \
    {                                                   \
      n[i].begin = values[i];                           \
      n[i].end = values[i];                             \
    }

START_TEST (test_insert_7)
{
  /*
   *                 [50]
   */

  DEF_TEST_SETUP ();
  interval_tree_insert (&tree, &N_50);
  ck_assert (N_50.color == ITREE_BLACK);
  ck_assert (&N_50 == tree.root);
}
END_TEST

START_TEST (test_insert_8)
{
  /*
   *                 [50]
   *                    \
   *                   (70)
   */

  DEF_TEST_SETUP ();
  interval_tree_insert (&tree, &N_50);
  interval_tree_insert (&tree, &N_70);
  ck_assert (N_50.color == ITREE_BLACK);
  ck_assert (N_70.color == ITREE_RED);
  ck_assert (&N_50 == tree.root);
  ck_assert (N_70.parent == &N_50);
  ck_assert (N_50.right == &N_70);
  ck_assert (N_50.left == &tree.nil);
  ck_assert (N_70.right == &tree.nil);
  ck_assert (N_70.left == &tree.nil);
}
END_TEST

START_TEST (test_insert_9)
{
  /* 3.a
   *                [70]
   *               /    \
   *             (50)   (80)
   */

  DEF_TEST_SETUP ();
  interval_tree_insert (&tree, &N_50);
  interval_tree_insert (&tree, &N_70);
  interval_tree_insert (&tree, &N_80);
  ck_assert (N_50.color == ITREE_RED);
  ck_assert (N_70.color == ITREE_BLACK);
  ck_assert (N_80.color == ITREE_RED);
  ck_assert (&N_70 == tree.root);
  ck_assert (N_50.parent == &N_70);
  ck_assert (N_70.right == &N_80);
  ck_assert (N_70.left == &N_50);
  ck_assert (N_80.right == &tree.nil);
  ck_assert (N_80.left == &tree.nil);
  ck_assert (N_80.parent == &N_70);
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

  DEF_TEST_SETUP ();
  interval_tree_insert (&tree, &N_50);
  interval_tree_insert (&tree, &N_70);
  interval_tree_insert (&tree, &N_80);
  interval_tree_insert (&tree, &N_90);
  ck_assert (N_50.color == ITREE_BLACK);
  ck_assert (N_70.color == ITREE_BLACK);
  ck_assert (N_80.color == ITREE_BLACK);
  ck_assert (N_90.color == ITREE_RED);
  ck_assert (&N_70 == tree.root);
  ck_assert (N_50.parent == &N_70);
  ck_assert (N_70.right == &N_80);
  ck_assert (N_70.left == &N_50);
  ck_assert (N_80.right == &N_90);
  ck_assert (N_80.left == &tree.nil);
  ck_assert (N_80.parent == &N_70);
  ck_assert (N_90.parent == &N_80);
  ck_assert (N_80.right == &N_90);
  ck_assert (N_90.left == &tree.nil);
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

  DEF_TEST_SETUP ();
  interval_tree_insert (&tree, &N_50);
  interval_tree_insert (&tree, &N_70);
  interval_tree_insert (&tree, &N_80);
  interval_tree_insert (&tree, &N_90);
  interval_tree_insert (&tree, &N_85);
  ck_assert (N_50.color == ITREE_BLACK);
  ck_assert (N_70.color == ITREE_BLACK);
  ck_assert (N_80.color == ITREE_RED);
  ck_assert (N_90.color == ITREE_RED);
  ck_assert (N_85.color == ITREE_BLACK);
  ck_assert (&N_70 == tree.root);
  ck_assert (N_50.parent == &N_70);
  ck_assert (N_70.right == &N_85);
  ck_assert (N_70.left == &N_50);
  ck_assert (N_80.right == &tree.nil);
  ck_assert (N_80.left == &tree.nil);
  ck_assert (N_80.parent == &N_85);
  ck_assert (N_90.parent == &N_85);
  ck_assert (N_80.right == &tree.nil);
  ck_assert (N_90.left == &tree.nil);
  ck_assert (N_85.right == &N_90);
  ck_assert (N_85.left == &N_80);
  ck_assert (N_85.parent == &N_70);

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

  DEF_TEST_SETUP ();
  interval_tree_insert (&tree, &N_50);
  interval_tree_insert (&tree, &N_70);
  interval_tree_insert (&tree, &N_80);
  interval_tree_insert (&tree, &N_90);
  interval_tree_insert (&tree, &N_85);
  interval_tree_insert (&tree, &N_95);
  ck_assert (N_50.color == ITREE_BLACK);
  ck_assert (N_70.color == ITREE_BLACK);
  ck_assert (N_80.color == ITREE_BLACK);
  ck_assert (N_90.color == ITREE_BLACK);
  ck_assert (N_85.color == ITREE_RED);
  ck_assert (N_95.color == ITREE_RED);
  ck_assert (&N_70 == tree.root);
  ck_assert (N_50.parent == &N_70);
  ck_assert (N_70.right == &N_85);
  ck_assert (N_70.left == &N_50);
  ck_assert (N_80.right == &tree.nil);
  ck_assert (N_80.left == &tree.nil);
  ck_assert (N_80.parent == &N_85);
  ck_assert (N_90.parent == &N_85);
  ck_assert (N_80.right == &tree.nil);
  ck_assert (N_90.left == &tree.nil);
  ck_assert (N_85.right == &N_90);
  ck_assert (N_85.left == &N_80);
  ck_assert (N_85.parent == &N_70);
  ck_assert (N_95.parent == &N_90);
  ck_assert (N_90.right == &N_95);
  ck_assert (N_95.left == &tree.nil);
}
END_TEST

#undef N_50
#undef N_70
#undef N_80
#undef N_90
#undef N_85
#undef N_95
#undef DEF_TEST_SETUP

struct interval_tree*
test_get_tree4 (struct interval_node **n)
{
  static struct interval_tree tree;
  static struct interval_node nodes[4];
  memset (&tree, 0, sizeof (struct interval_tree));
  memset (&nodes, 0, 4 * sizeof (struct interval_node));
  interval_tree_init (&tree);
  for (int i = 0; i < 4; ++i)
    {
      nodes[i].begin = 10 * (i + 1);
      nodes[i].end = nodes[i].begin;
      interval_tree_insert (&tree, &nodes[i]);
    }
  *n = nodes;
  return &tree;
}

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

#define N_10 (nodes[0])
#define N_20 (nodes[1])
#define N_30 (nodes[2])
#define N_40 (nodes[3])

START_TEST (test_insert_13)
{
  struct interval_node *nodes = NULL;
  struct interval_tree *tree = test_get_tree4 (&nodes);


  ck_assert (tree->root == &N_20);
  ck_assert (N_20.left == &N_10);
  ck_assert (N_20.right == &N_30);
  ck_assert (N_30.right == &N_40);
  ck_assert (N_10.color == ITREE_BLACK);
  ck_assert (N_20.color == ITREE_BLACK);
  ck_assert (N_30.color == ITREE_BLACK);
  ck_assert (N_40.color == ITREE_RED);
}
END_TEST

START_TEST (test_insert_14)
{
  struct interval_tree tree;
  struct interval_node nodes[3];

  nodes[0].begin = nodes[1].begin = nodes[2].begin = 10;
  nodes[0].end = nodes[1].end = nodes[2].end = 10;

  for (int i = 0; i < 3; ++i)
    interval_tree_insert (&tree, &nodes[i]);
  for (int i = 0; i < 3; ++i)
    ck_assert (interval_tree_contains (&tree, &nodes[i]));
}
END_TEST




/* +===================================================================================+
 * | Remove
 * +===================================================================================+ */

#define A (nodes[0])
#define B (nodes[1])
#define C (nodes[2])
#define D (nodes[3])
#define E (nodes[4])

/* Creating proper test trees for the formal tests via insertions is
   way to tedious, so we just fake it and only test the
   fix-routine. */
#define DEF_TEST_SETUP()                                        \
    struct interval_tree tree;                                  \
    struct interval_node nodes[5];                              \
    interval_tree_init (&tree);                                 \
    tree.root = &B;                                             \
    A.parent = &B; B.parent = &tree.nil; C.parent = &D;         \
    D.parent = &B; E.parent = &D;                               \
    A.left = A.right = C.left = C.right = &tree.nil;            \
    E.left = E.right = &tree.nil;                               \
    B.left = &A; B.right = &D; D.left = &C; D.right = &E        \

/* 1.a -> 2.a
 *                [B]
 *               /    \
 *             [A]    (D)
 *                    /  \
 *                 [C]   [E]
 */


START_TEST (test_remove_1)
{
  DEF_TEST_SETUP ();
  B.color = A.color = C.color = E.color = ITREE_BLACK;
  D.color = ITREE_RED;
  interval_tree_remove_fix (&tree, &A);

  ck_assert (A.color == ITREE_BLACK);
  ck_assert (B.color == ITREE_BLACK);
  ck_assert (C.color == ITREE_RED);
  ck_assert (D.color == ITREE_BLACK);
  ck_assert (E.color == ITREE_BLACK);
  ck_assert (A.parent == &B);
  ck_assert (B.left == &A);
  ck_assert (B.right == &C);
  ck_assert (C.parent == &B);
  ck_assert (E.parent == &D);
  ck_assert (D.right == &E);
  ck_assert (D.left == &B);
  ck_assert (tree.root == &D);
}
END_TEST

/* 2.a */
START_TEST (test_remove_2)
{
  DEF_TEST_SETUP ();
  B.color = D.color = A.color = C.color = E.color = ITREE_BLACK;
  interval_tree_remove_fix (&tree, &A);

  ck_assert (A.color == ITREE_BLACK);
  ck_assert (B.color == ITREE_BLACK);
  ck_assert (C.color == ITREE_BLACK);
  ck_assert (D.color == ITREE_RED);
  ck_assert (E.color == ITREE_BLACK);
  ck_assert (A.parent == &B);
  ck_assert (B.left == &A);
  ck_assert (B.right == &D);
  ck_assert (C.parent == &D);
  ck_assert (E.parent == &D);
  ck_assert (tree.root == &B);
}
END_TEST

/* 3.a -> 4.a*/
START_TEST (test_remove_3)
{
  DEF_TEST_SETUP ();
  D.color = A.color = E.color = ITREE_BLACK;
  B.color = C.color = ITREE_RED;
  interval_tree_remove_fix (&tree, &A);

  ck_assert (A.color == ITREE_BLACK);
  ck_assert (B.color == ITREE_BLACK);
  ck_assert (C.color == ITREE_BLACK);
  ck_assert (D.color == ITREE_BLACK);
  ck_assert (E.color == ITREE_BLACK);
  ck_assert (A.parent == &B);
  ck_assert (B.left == &A);
  ck_assert (B.right == &tree.nil);
  ck_assert (&C == tree.root);
  ck_assert (C.left == &B);
  ck_assert (C.right == &D);
  ck_assert (E.parent == &D);
  ck_assert (D.left == &tree.nil);

}
END_TEST

/* 4.a */
START_TEST (test_remove_4)
{
  DEF_TEST_SETUP ();
  B.color = C.color = E.color = ITREE_RED;
  A.color = D.color = ITREE_BLACK;
  interval_tree_remove_fix (&tree, &A);

  ck_assert (A.color == ITREE_BLACK);
  ck_assert (B.color == ITREE_BLACK);
  ck_assert (C.color == ITREE_RED);
  ck_assert (D.color == ITREE_BLACK);
  ck_assert (E.color == ITREE_BLACK);
  ck_assert (A.parent == &B);
  ck_assert (B.left == &A);
  ck_assert (B.right == &C);
  ck_assert (C.parent == &B);
  ck_assert (E.parent == &D);
  ck_assert (tree.root == &D);
}
END_TEST


#undef A
#undef B
#undef C
#undef D
#undef E
#undef DEF_TEST_SETUP



/* These are the mirrored cases. */

#define A (nodes[0])
#define B (nodes[1])
#define C (nodes[2])
#define D (nodes[3])
#define E (nodes[4])

#define DEF_TEST_SETUP()                                        \
    struct interval_tree tree;                                  \
    struct interval_node nodes[5];                              \
    interval_tree_init (&tree);                                 \
    tree.root = &B;                                             \
    A.parent = &B; B.parent = &tree.nil; C.parent = &D;         \
    D.parent = &B; E.parent = &D;                               \
    A.right = A.left = C.right = C.left = &tree.nil;            \
    E.right = E.left = &tree.nil;                               \
    B.right = &A; B.left = &D; D.right = &C; D.left = &E        \

/* 1.b -> 2.b
 *                [B]
 *               /    \
 *             [A]    (D)
 *                    /  \
 *                 [C]   [E]
 */


START_TEST (test_remove_5)
{
  DEF_TEST_SETUP ();
  B.color = A.color = C.color = E.color = ITREE_BLACK;
  D.color = ITREE_RED;
  interval_tree_remove_fix (&tree, &A);

  ck_assert (A.color == ITREE_BLACK);
  ck_assert (B.color == ITREE_BLACK);
  ck_assert (C.color == ITREE_RED);
  ck_assert (D.color == ITREE_BLACK);
  ck_assert (E.color == ITREE_BLACK);
  ck_assert (A.parent == &B);
  ck_assert (B.right == &A);
  ck_assert (B.left == &C);
  ck_assert (C.parent == &B);
  ck_assert (E.parent == &D);
  ck_assert (D.left == &E);
  ck_assert (D.right == &B);
  ck_assert (tree.root == &D);
}
END_TEST

/* 2.b */
START_TEST (test_remove_6)
{
  DEF_TEST_SETUP ();
  B.color = D.color = A.color = C.color = E.color = ITREE_BLACK;
  interval_tree_remove_fix (&tree, &A);

  ck_assert (A.color == ITREE_BLACK);
  ck_assert (B.color == ITREE_BLACK);
  ck_assert (C.color == ITREE_BLACK);
  ck_assert (D.color == ITREE_RED);
  ck_assert (E.color == ITREE_BLACK);
  ck_assert (A.parent == &B);
  ck_assert (B.right == &A);
  ck_assert (B.left == &D);
  ck_assert (C.parent == &D);
  ck_assert (E.parent == &D);
  ck_assert (tree.root == &B);
}
END_TEST

/* 3.b -> 4.b*/
START_TEST (test_remove_7)
{
  DEF_TEST_SETUP ();
  D.color = A.color = E.color = ITREE_BLACK;
  B.color = C.color = ITREE_RED;
  interval_tree_remove_fix (&tree, &A);

  ck_assert (A.color == ITREE_BLACK);
  ck_assert (B.color == ITREE_BLACK);
  ck_assert (C.color == ITREE_BLACK);
  ck_assert (D.color == ITREE_BLACK);
  ck_assert (E.color == ITREE_BLACK);
  ck_assert (A.parent == &B);
  ck_assert (B.right == &A);
  ck_assert (B.left == &tree.nil);
  ck_assert (&C == tree.root);
  ck_assert (C.right == &B);
  ck_assert (C.left == &D);
  ck_assert (E.parent == &D);
  ck_assert (D.right == &tree.nil);

}
END_TEST

/* 4.b */
START_TEST (test_remove_8)
{
  DEF_TEST_SETUP ();
  B.color = C.color = E.color = ITREE_RED;
  A.color = D.color = ITREE_BLACK;
  interval_tree_remove_fix (&tree, &A);

  ck_assert (A.color == ITREE_BLACK);
  ck_assert (B.color == ITREE_BLACK);
  ck_assert (C.color == ITREE_RED);
  ck_assert (D.color == ITREE_BLACK);
  ck_assert (E.color == ITREE_BLACK);
  ck_assert (A.parent == &B);
  ck_assert (B.right == &A);
  ck_assert (B.left == &C);
  ck_assert (C.parent == &B);
  ck_assert (E.parent == &D);
  ck_assert (tree.root == &D);
}
END_TEST


#undef A
#undef B
#undef C
#undef D
#undef E
#undef DEF_TEST_SETUP


START_TEST (test_remove_9)
{
  struct interval_node *nodes = NULL;
  struct interval_tree *tree = test_get_tree4 (&nodes);

  ck_assert (tree->root == &N_20);
  ck_assert (N_20.left == &N_10);
  ck_assert (N_20.right == &N_30);
  ck_assert (N_30.right == &N_40);
  ck_assert (N_20.color == ITREE_BLACK);
  ck_assert (N_10.color == ITREE_BLACK);
  ck_assert (N_30.color == ITREE_BLACK);
  ck_assert (N_40.color == ITREE_RED);

  interval_tree_remove (tree, &N_10);

  ck_assert (tree->root == &N_30);
  ck_assert (N_30.parent == &tree->nil);
  ck_assert (N_30.left == &N_20);
  ck_assert (N_30.right == &N_40);
  ck_assert (N_20.color == ITREE_BLACK);
  ck_assert (N_30.color == ITREE_BLACK);
  ck_assert (N_40.color == ITREE_BLACK);
}
END_TEST

#define N 3

START_TEST (test_remove_10)
{
  struct interval_tree tree;
  struct interval_node nodes[N];
  int index[N];

  srand (42);
  interval_tree_init (&tree);
  for (int i = 0; i < N; ++i)
    {
      nodes[i].begin = (i + 1) * 10;
      nodes[i].end = nodes[i].begin + 1;
      index[i] = i;
    }
  shuffle (index, N);
  for (int i = 0; i < N; ++i)
    interval_tree_insert (&tree, &nodes[index[i]]);

  shuffle (index, N);
  for (int i = 0; i < N; ++i)
    {
      ck_assert (interval_tree_contains (&tree, &nodes[index[i]]));
      interval_tree_remove (&tree, &nodes[index[i]]);
    }
  ck_assert (tree.root == &tree.nil);
  ck_assert (tree.size == 0);
}
END_TEST


/* +===================================================================================+
 * | Generator
 * +===================================================================================+ */

START_TEST (test_generator_1)
{
  struct interval_tree tree;
  struct interval_node node, *n;
  struct interval_generator *g;
  interval_tree_init (&tree);
  node.begin = 10;
  node.end = 20;
  interval_tree_insert (&tree, &node);
  g = interval_generator_create (&tree);
  interval_generator_reset (g, 0, 30, ITREE_ASCENDING);
  n = interval_generator_next (g);
  ck_assert (n == &node);
  ck_assert (n->begin == 10 && n->end == 20);
  ck_assert (interval_generator_next (g) == NULL);
  ck_assert (interval_generator_next (g) == NULL);
  ck_assert (interval_generator_next (g) == NULL);
  interval_generator_destroy (g);

  g = interval_generator_create (&tree);
  interval_generator_reset (g, 30, 50, ITREE_ASCENDING);
  ck_assert (interval_generator_next (g) == NULL);
  ck_assert (interval_generator_next (g) == NULL);
  ck_assert (interval_generator_next (g) == NULL);
  interval_generator_destroy (g);
}
END_TEST

void
test_check_generator (struct interval_tree *tree,
                      ptrdiff_t begin, ptrdiff_t end,
                      int n, ...)
{
  va_list ap;
  struct interval_generator *g = interval_generator_create (tree);
  interval_generator_reset (g, begin, end, ITREE_ASCENDING);

  va_start (ap, n);
  for (int i = 0; i < n; ++i)
    {
      ptrdiff_t begin = va_arg (ap, ptrdiff_t);
      struct interval_node *node = interval_generator_next (g);
      ck_assert (node);
      ck_assert_int_eq (node->begin, begin);
    }
  va_end (ap);
  ck_assert (! interval_generator_next (g));
  ck_assert (! interval_generator_next (g));
  interval_generator_destroy (g);
}

#define DEF_TEST_SETUP()                        \


START_TEST (test_generator_2)
{
  struct interval_tree tree;
  struct interval_node nodes[3];

  interval_tree_init (&tree);

  for (int i = 0; i < 3; ++i) {
    nodes[i].begin = 10 * (i + 1);
    nodes[i].end = 10 * (i + 2);
    interval_tree_insert (&tree, &nodes[i]);
  }

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


struct interval_node*
test_create_tree (struct interval_tree *tree, int n,
                  bool doshuffle, ...)
{
  va_list ap;
  struct interval_node *nodes = calloc (n, sizeof (struct interval_node));
  int *index = calloc (n, sizeof (int));

  interval_tree_init (tree);
  va_start (ap, doshuffle);
  for (int i = 0; i < n; ++i)
    {
      ptrdiff_t begin = va_arg (ap, ptrdiff_t);
      ptrdiff_t end = va_arg (ap, ptrdiff_t);
      nodes[i].begin = begin;
      nodes[i].end = end;
      index[i] = i;
    }
  va_end (ap);
  srand (42);
  if (doshuffle)
    shuffle (index, n);
  for (int i = 0; i < n; ++i)
    interval_tree_insert (tree, &nodes[index[i]]);
  free (index);

  return nodes;
}

START_TEST (test_generator_3)
{
  struct interval_tree tree;
  struct interval_node *nodes = NULL;

  nodes = test_create_tree (&tree, 3, true,
                            10, 10,
                            10, 10,
                            10, 10);
  test_check_generator (&tree, 0, 10, 0);
  test_check_generator (&tree, 10, 10, 3, 10, 10, 10);
  test_check_generator (&tree, 10, 20, 3, 10, 10, 10);
  free (nodes);
}
END_TEST

#define FOREACH(n, g)                                   \
  for ((n) = interval_generator_next (g); (n) != NULL;  \
       (n) = interval_generator_next (g))

START_TEST (test_generator_5)
{
  struct interval_tree tree;
  struct interval_node *nodes;
  struct interval_generator *g;
  nodes = test_create_tree (&tree, 4, false,
                            10, 30,
                            20, 40,
                            30, 50,
                            40, 60);
  g = interval_generator_create (&tree);
  interval_generator_reset (g, 0, 100, ITREE_PRE_ORDER);
  for (int i = 0; i < 4; ++i)
    {
      struct interval_node *n = interval_generator_next (g);
      ck_assert (n);
      switch (i)
        {
        case 0: ck_assert_int_eq (20, n->begin); break;
        case 1: ck_assert_int_eq (10, n->begin); break;
        case 2: ck_assert_int_eq (30, n->begin); break;
        case 3: ck_assert_int_eq (40, n->begin); break;
        }
    }
  interval_generator_destroy (g);
  free (nodes);

}
END_TEST

START_TEST (test_generator_6)
{
  struct interval_tree tree;
  struct interval_node *nodes;
  struct interval_generator *g;
  nodes = test_create_tree (&tree, 4, true,
                            10, 30,
                            20, 40,
                            30, 50,
                            40, 60);
  g = interval_generator_create (&tree);
  interval_generator_reset (g, 0, 100, ITREE_ASCENDING);
  for (int i = 0; i < 4; ++i)
    {
      struct interval_node *n = interval_generator_next (g);
      ck_assert (n);
      switch (i)
        {
        case 0: ck_assert_int_eq (10, n->begin); break;
        case 1: ck_assert_int_eq (20, n->begin); break;
        case 2: ck_assert_int_eq (30, n->begin); break;
        case 3: ck_assert_int_eq (40, n->begin); break;
        }
    }
  interval_generator_destroy (g);
  free (nodes);

}
END_TEST

START_TEST (test_generator_7)
{
  struct interval_tree tree;
  struct interval_node *nodes;
  struct interval_generator *g;
  nodes = test_create_tree (&tree, 4, true,
                            10, 30,
                            20, 40,
                            30, 50,
                            40, 60);
  g = interval_generator_create (&tree);
  interval_generator_reset (g, 0, 100, ITREE_DESCENDING);
  for (int i = 0; i < 4; ++i)
    {
      struct interval_node *n = interval_generator_next (g);
      ck_assert (n);
      switch (i)
        {
        case 0: ck_assert_int_eq (40, n->begin); break;
        case 1: ck_assert_int_eq (30, n->begin); break;
        case 2: ck_assert_int_eq (20, n->begin); break;
        case 3: ck_assert_int_eq (10, n->begin); break;
        }
    }
  interval_generator_destroy (g);
  free (nodes);

}
END_TEST

START_TEST (test_generator_8)
{
  struct interval_tree tree;
  struct interval_node *nodes, *n;
  struct interval_generator *g;
  nodes = test_create_tree (&tree, 2, false,
                            20, 30,
                            40, 50);
  g = interval_generator_create (&tree);
  interval_generator_reset (g, 1, 60, ITREE_DESCENDING);
  n = interval_generator_next (g);
  ck_assert_int_eq (n->begin, 40);
  interval_generator_narrow (g, 50, 60);
  n = interval_generator_next (g);
  ck_assert (n == NULL);
  free (nodes);
}
END_TEST


START_TEST (test_generator_9)
{
  struct interval_tree tree;
  struct interval_node *nodes, *n;
  struct interval_generator *g;
  nodes = test_create_tree (&tree, 2, false,
                            25, 25,
                            20, 30);
  g = interval_generator_create (&tree);
  interval_generator_reset (g, 1, 30, ITREE_DESCENDING);
  n = interval_generator_next (g);
  ck_assert_int_eq (n->begin, 25);
  interval_generator_narrow (g, 25, 35);
  n = interval_generator_next (g);
  ck_assert_int_eq (n->begin, 20);
  free (nodes);
}
END_TEST


/* +===================================================================================+
 * | Insert Gap
 * +===================================================================================+ */

static struct interval_tree gap_tree;
static struct interval_node gap_node;

#define N_BEG (interval_tree_validate (&gap_tree, &gap_node)->begin)
#define N_END (interval_tree_validate (&gap_tree, &gap_node)->end)

static void
test_setup_gap_node (ptrdiff_t begin, ptrdiff_t end,
                     bool front_advance, bool rear_advance)
{
  interval_tree_init (&gap_tree);
  gap_node.begin = begin;
  gap_node.end = end;
  gap_node.front_advance = front_advance;
  gap_node.rear_advance = rear_advance;
  interval_tree_insert (&gap_tree, &gap_node);
}

static void
test_setup_gap_node_noadvance (ptrdiff_t begin, ptrdiff_t end)
{
  test_setup_gap_node (begin, end, false, false);
}

START_TEST (test_gap_insert_1)
{
  test_setup_gap_node (100, 200, false, false);
  interval_tree_insert_gap (&gap_tree, 100 + 10, 20);
  ck_assert_int_eq (N_BEG, 100);
  ck_assert_int_eq (N_END, 200 + 20);
}
END_TEST

START_TEST (test_gap_insert_2)
{
  test_setup_gap_node (100, 200, false, false);
  interval_tree_insert_gap (&gap_tree, 300, 10);
  ck_assert_int_eq (N_BEG, 100);
  ck_assert_int_eq (N_END, 200);
}
END_TEST

START_TEST (test_gap_insert_3)
{
  test_setup_gap_node (100, 200, false, false);
  interval_tree_insert_gap (&gap_tree, 0, 15);
  ck_assert_int_eq (N_BEG, 100 + 15);
  ck_assert_int_eq (N_END, 200 + 15);
}
END_TEST

START_TEST (test_gap_insert_4)
{
  test_setup_gap_node (100, 200, true, false);
  interval_tree_insert_gap (&gap_tree, 100, 20);
  ck_assert_int_eq (N_BEG, 100 + 20);
  ck_assert_int_eq (N_END, 200 + 20);

}
END_TEST

START_TEST (test_gap_insert_5)
{
  test_setup_gap_node (100, 200, false, false);
  interval_tree_insert_gap (&gap_tree, 100, 20);
  ck_assert_int_eq (N_BEG, 100);
  ck_assert_int_eq (N_END, 200 + 20);

}
END_TEST

START_TEST (test_gap_insert_6)
{
  test_setup_gap_node (100, 200, false, true);
  interval_tree_insert_gap (&gap_tree, 200, 20);
  ck_assert_int_eq (N_BEG, 100);
  ck_assert_int_eq (N_END, 200 + 20);

}
END_TEST

START_TEST (test_gap_insert_7)
{
  test_setup_gap_node (100, 200, false, false);
  interval_tree_insert_gap (&gap_tree, 200, 20);
  ck_assert_int_eq (N_BEG, 100);
  ck_assert_int_eq (N_END, 200);

}
END_TEST

START_TEST (test_gap_insert_8)
{
  test_setup_gap_node (100, 100, true, true);
  interval_tree_insert_gap (&gap_tree, 100, 20);
  ck_assert_int_eq (N_BEG, 100 + 20);
  ck_assert_int_eq (N_END, 100 + 20);

}
END_TEST

START_TEST (test_gap_insert_9)
{
  test_setup_gap_node (100, 100, false, true);
  interval_tree_insert_gap (&gap_tree, 100, 20);
  ck_assert_int_eq (N_BEG, 100);
  ck_assert_int_eq (N_END, 100 + 20);

}
END_TEST

START_TEST (test_gap_insert_10)
{
  test_setup_gap_node (100, 100, true, false);
  interval_tree_insert_gap (&gap_tree, 100, 20);
  ck_assert_int_eq (N_BEG, 100);
  ck_assert_int_eq (N_END, 100);

}
END_TEST

START_TEST (test_gap_insert_11)
{
  test_setup_gap_node (100, 100, false, false);
  interval_tree_insert_gap (&gap_tree, 100, 20);
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
  interval_tree_delete_gap (&gap_tree, 100 + 10, 20);
  ck_assert_int_eq (N_BEG, 100);
  ck_assert_int_eq (N_END, 200 - 20);

}
END_TEST

START_TEST (test_gap_delete_2)
{
  test_setup_gap_node_noadvance (100, 200);
  interval_tree_delete_gap (&gap_tree, 200 + 10, 20);
  ck_assert_int_eq (N_BEG, 100);
  ck_assert_int_eq (N_END, 200);

}
END_TEST

START_TEST (test_gap_delete_3)
{
  test_setup_gap_node_noadvance (100, 200);
  interval_tree_delete_gap (&gap_tree, 200, 20);
  ck_assert_int_eq (N_BEG, 100);
  ck_assert_int_eq (N_END, 200);

}
END_TEST

START_TEST (test_gap_delete_4)
{
  test_setup_gap_node_noadvance (100, 200);
  interval_tree_delete_gap (&gap_tree, 100 - 20, 20);
  ck_assert_int_eq (N_BEG, 100 - 20);
  ck_assert_int_eq (N_END, 200 - 20);

}
END_TEST

START_TEST (test_gap_delete_5)
{
  test_setup_gap_node_noadvance (100, 200);
  interval_tree_delete_gap (&gap_tree, 70, 20);
  ck_assert_int_eq (N_BEG, 100 - 20);
  ck_assert_int_eq (N_END, 200 - 20);

}
END_TEST

START_TEST (test_gap_delete_6)
{
  test_setup_gap_node_noadvance (100, 200);
  interval_tree_delete_gap (&gap_tree, 80, 100);
  ck_assert_int_eq (N_BEG, 80);
  ck_assert_int_eq (N_END, 100);
}
END_TEST

START_TEST (test_gap_delete_7)
{
  test_setup_gap_node_noadvance (100, 200);
  interval_tree_delete_gap (&gap_tree, 120, 100);
  ck_assert_int_eq (N_BEG, 100);
  ck_assert_int_eq (N_END, 120);
}
END_TEST

START_TEST (test_gap_delete_8)
{
  test_setup_gap_node_noadvance (100, 200);
  interval_tree_delete_gap (&gap_tree, 100 - 20, 200 + 20);
  ck_assert_int_eq (N_BEG, 100 - 20);
  ck_assert_int_eq (N_END, 100 - 20);

}
END_TEST



Suite * basic_suite ()
{
  Suite *s = suite_create ("basic_suite");
  TCase *tc = tcase_create ("basic_test");

  tcase_add_test (tc, test_insert_1);
  tcase_add_test (tc, test_insert_2);
  tcase_add_test (tc, test_insert_3);
  tcase_add_test (tc, test_insert_4);
  tcase_add_test (tc, test_insert_5);
  tcase_add_test (tc, test_insert_6);
  tcase_add_test (tc, test_insert_7);
  tcase_add_test (tc, test_insert_8);
  tcase_add_test (tc, test_insert_9);
  tcase_add_test (tc, test_insert_10);
  tcase_add_test (tc, test_insert_11);
  tcase_add_test (tc, test_insert_12);
  tcase_add_test (tc, test_insert_13);

  tcase_add_test (tc, test_remove_1);
  tcase_add_test (tc, test_remove_2);
  tcase_add_test (tc, test_remove_3);
  tcase_add_test (tc, test_remove_4);
  tcase_add_test (tc, test_remove_5);
  tcase_add_test (tc, test_remove_6);
  tcase_add_test (tc, test_remove_7);
  tcase_add_test (tc, test_remove_8);
  tcase_add_test (tc, test_remove_9);
  tcase_add_test (tc, test_remove_10);

  tcase_add_test (tc, test_generator_1);
  tcase_add_test (tc, test_generator_2);
  tcase_add_test (tc, test_generator_3);
  tcase_add_test (tc, test_generator_5);
  tcase_add_test (tc, test_generator_6);
  tcase_add_test (tc, test_generator_7);
  tcase_add_test (tc, test_generator_8);
  tcase_add_test (tc, test_generator_9);

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

  tcase_add_test (tc, test_gap_delete_1);
  tcase_add_test (tc, test_gap_delete_2);
  tcase_add_test (tc, test_gap_delete_3);
  tcase_add_test (tc, test_gap_delete_4);
  tcase_add_test (tc, test_gap_delete_5);
  tcase_add_test (tc, test_gap_delete_6);
  tcase_add_test (tc, test_gap_delete_7);
  tcase_add_test (tc, test_gap_delete_8);

  /* tcase_set_timeout (tc, 120); */
  suite_add_tcase (s, tc);
  return s;
}

int
main (void)
{
  int nfailed;
  Suite *s = basic_suite ();
  SRunner *sr = srunner_create (s);

  srunner_run_all (sr, CK_NORMAL);
  nfailed = srunner_ntests_failed (sr);
  srunner_free (sr);
  return (nfailed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
