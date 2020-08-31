/* awluthe.c: POOL CLASS AWL UNIT TEST WITH OBJECT HEADERS
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 *
 * DESIGN
 *
 * .design: see <design/poolawl#.test>
 */

#include "mpscawl.h"
#include "mpsclo.h"
#include "mpsavm.h"
#include "fmthe.h"
#include "fmtdy.h"
#include "testlib.h"
#include "testthr.h"
#include "mpslib.h"
#include "mps.h"
#include "mpstd.h"

#include <string.h> /* strlen */
#include <stdio.h> /* printf */


#define testArenaSIZE     ((size_t)64<<20)
#define TABLE_SLOTS 49
#define ITERATIONS 5000
#define CHATTER 100


static mps_word_t bogus_class;

#define UNINIT 0x041412ED

#define DYLAN_ALIGN 4 /* depends on value defined in fmtdy.c */


/* size_tAlignUp -- align w up to alignment a */

#define size_tAlignUp(w, a) (((w) + (a) - 1) & ~((size_t)(a) - 1))


static mps_word_t wrapper_wrapper[] = {
  UNINIT,                       /* wrapper */
  UNINIT,                       /* class */
  0,                            /* Extra word */
  (mps_word_t)4<<2|2,                     /* F */
  (mps_word_t)2<<(MPS_WORD_WIDTH - 8),    /* V */
  (mps_word_t)1<<2|1,                     /* VL */
  1                             /* patterns */
};


static mps_word_t string_wrapper[] = {
  UNINIT,                       /* wrapper */
  UNINIT,                       /* class */
  0,                            /* extra word */
  0,                            /* F */
  (mps_word_t)2<<(MPS_WORD_WIDTH - 8)|(mps_word_t)3<<3|4,   /* V */
  1                             /* VL */
};

static mps_word_t table_wrapper[] = {
  UNINIT,                       /* wrapper */
  UNINIT,                       /* class */
  0,                            /* extra word */
  (mps_word_t)1<<2|1,                     /* F */
  (mps_word_t)2<<(MPS_WORD_WIDTH - 8)|2,  /* V */
  1                             /* VL */
};


static void initialise_wrapper(mps_word_t *wrapper)
{
  wrapper[0] = (mps_word_t)&wrapper_wrapper;
  wrapper[1] = (mps_word_t)&bogus_class;
}


/* alloc_string  - create a dylan string object
 *
 * create a dylan string object (byte vector) whose contents
 * are the string s (including the terminating NUL)
 * .assume.dylan-obj
 */

static mps_word_t *alloc_string(const char *s, mps_ap_t ap)
{
  size_t l;
  size_t objsize;
  void *p;
  mps_word_t *object;

  l = strlen(s)+1;
  /* number of words * sizeof word */
  objsize = (2 + (l+sizeof(mps_word_t)-1)/sizeof(mps_word_t))
            * sizeof(mps_word_t);
  objsize = size_tAlignUp(objsize, DYLAN_ALIGN);
  do {
    size_t i;
    char *s2;

    die(mps_reserve(&p, ap, objsize + headerSIZE), "Reserve Leaf\n");
    object = (mps_word_t *)((char *)p + headerSIZE);
    object[0] = (mps_word_t)string_wrapper;
    object[1] = l << 2 | 1;
    s2 = (char *)&object[2];
    for(i = 0; i < l; ++i) {
      s2[i] = s[i];
    }
    ((int*)p)[0] = realHeader;
    ((int*)p)[1] = 0xED0ED;
  } while(!mps_commit(ap, p, objsize + headerSIZE));
  return object;
}


/* alloc_table -- create a table with n variable slots
 *
 * .assume.dylan-obj
 */

static mps_word_t *alloc_table(size_t n, mps_ap_t ap)
{
  size_t objsize;
  void *p;
  mps_word_t *object;

  objsize = (3 + n) * sizeof(mps_word_t);
  objsize = size_tAlignUp(objsize, MPS_PF_ALIGN);
  do {
    size_t i;

    die(mps_reserve(&p, ap, objsize + headerSIZE), "Reserve Table\n");
    object = (mps_word_t *)((char *)p + headerSIZE);
    object[0] = (mps_word_t)table_wrapper;
    object[1] = 0;
    object[2] = n << 2 | 1;
    for(i = 0; i < n; ++i) {
      object[3+i] = 0;
    }
    ((int*)p)[0] = realHeader;
    ((int*)p)[1] = 0xED0ED;
  } while(!mps_commit(ap, p, objsize + headerSIZE));
  return object;
}


/* gets the nth slot from a table
 * .assume.dylan-obj
 */
static mps_word_t *table_slot(mps_word_t *table, size_t n)
{
  return (mps_word_t *)table[3+n];
}


/* sets the nth slot in a table
 * .assume.dylan-obj
 */
static void set_table_slot(mps_word_t *table, size_t n, mps_word_t *p)
{
  cdie(table[0] == (mps_word_t)table_wrapper, "set_table_slot");
  table[3+n] = (mps_word_t)p;
}


/* links two tables together via their link slot
 * (1st fixed part slot)
 */
static void table_link(mps_word_t *t1, mps_word_t *t2)
{
  cdie(t1[0] == (mps_word_t)table_wrapper, "table_link 1");
  cdie(t2[0] == (mps_word_t)table_wrapper, "table_link 2");
  t1[1] = (mps_word_t)t2;
  t2[1] = (mps_word_t)t1;
}


typedef struct tables_s {
  mps_arena_t arena;
  mps_word_t *weaktable;
  mps_word_t *exacttable;
  mps_word_t *preserve[TABLE_SLOTS];    /* preserves objects in the weak */
                                        /* table by referring to them */
  mps_ap_t weakap, exactap, bogusap, leafap;
} tables_s, *tables_t;

/* populate -- populate the weak table in a thread
 *
 * We use a thread to populate the table to avoid leaving any
 * references to objects in the table in registers, so that we can
 * test their weakness properly.
 */

static void *populate(void *state)
{
  tables_t tables = state;
  size_t i;
  mps_thr_t me;
  mps_root_t root;

  die(mps_thread_reg(&me, tables->arena), "mps_thread_reg(populate)");
  die(mps_root_create_thread(&root, tables->arena, me, &state), "mps_root_create_thread(populate)");

  tables->exacttable = alloc_table(TABLE_SLOTS, tables->exactap);
  tables->weaktable = alloc_table(TABLE_SLOTS, tables->weakap);
  table_link(tables->exacttable, tables->weaktable);

  for(i = 0; i < TABLE_SLOTS; ++i) {
    mps_word_t *string;
    if (rnd() % 2 == 0) {
      string = alloc_string("iamalive", tables->leafap);
      tables->preserve[i] = string;
    } else {
      string = alloc_string("iamdead", tables->leafap);
      tables->preserve[i] = 0;
    }
    set_table_slot(tables->weaktable, i, string);
    string = alloc_string("iamexact", tables->leafap);
    set_table_slot(tables->exacttable, i, string);
  }

  mps_root_destroy(root);
  mps_thread_dereg(me);

  return NULL;
}

static void test(mps_arena_t arena,
                 mps_ap_t leafap, mps_ap_t exactap, mps_ap_t weakap,
                 mps_ap_t bogusap)
{
  tables_s tables;
  size_t i, j;
  testthr_t thr;
  void *p;

  /* Leave bogusap between reserve and commit for the duration */
  die(mps_reserve(&p, bogusap, 64), "Reserve bogus");

  tables.arena = arena;
  tables.exactap = exactap;
  tables.weakap = weakap;
  tables.leafap = leafap;
  tables.bogusap = bogusap;

  /* We using a thread for its pararallel execution, so just create
     and wait for it to finish. */
  testthr_create(&thr, populate, &tables);
  testthr_join(&thr, NULL);

  for(j = 0; j < ITERATIONS; ++j) {
    for(i = 0; i < TABLE_SLOTS; ++i) {
      (void)alloc_string("spong", leafap);
    }
  }

  die(mps_arena_collect(arena), "mps_arena_collect");
  mps_arena_release(arena);

  for(i = 0; i < TABLE_SLOTS; ++i) {
    if (tables.preserve[i] == 0) {
      if (table_slot(tables.weaktable, i)) {
        error("Strongly unreachable weak table entry found, "
              "slot %"PRIuLONGEST".\n", (ulongest_t)i);
      } else {
        if (table_slot(tables.exacttable, i) != 0) {
          error("Weak table entry deleted, but corresponding "
                "exact table entry not deleted, slot %"PRIuLONGEST".\n",
                (ulongest_t)i);
        }
      }
    }
  }

  (void)mps_commit(bogusap, p, 64);
}


/* setup -- set up pools for the test
 *
 * guff serves two purposes:
 *  - a pseudo stack base for the stack root.
 *  - pointer to a guff structure, which packages some values needed
 *   (arena and thr mostly)
 */

struct guff_s {
  mps_arena_t arena;
  mps_thr_t thr;
};

ATTRIBUTE_NOINLINE
static void setup(struct guff_s *guff)
{
  mps_arena_t arena;
  mps_pool_t leafpool;
  mps_pool_t tablepool;
  mps_fmt_t dylanfmt;
  mps_fmt_t dylanweakfmt;
  mps_ap_t leafap, exactap, weakap, bogusap;
  mps_root_t stack;
  mps_thr_t thr;

  arena = guff->arena;
  thr = guff->thr;

  die(mps_root_create_thread(&stack, arena, thr, guff),
      "Root Create\n");
  die(EnsureHeaderFormat(&dylanfmt, arena), "EnsureHeaderFormat");
  die(EnsureHeaderWeakFormat(&dylanweakfmt, arena), "EnsureHeaderWeakFormat");
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, dylanfmt);
    die(mps_pool_create_k(&leafpool, arena, mps_class_lo(), args),
        "Leaf Pool Create\n");
  } MPS_ARGS_END(args);
  die(mps_pool_create(&tablepool, arena, mps_class_awl(), dylanweakfmt,
                      dylan_weak_dependent),
      "Table Pool Create\n");
  die(mps_ap_create(&leafap, leafpool, mps_rank_exact()),
      "Leaf AP Create\n");
  die(mps_ap_create(&exactap, tablepool, mps_rank_exact()),
      "Exact AP Create\n");
  die(mps_ap_create(&weakap, tablepool, mps_rank_weak()),
      "Weak AP Create\n");
  die(mps_ap_create(&bogusap, tablepool, mps_rank_exact()),
      "Bogus AP Create\n");

  test(arena, leafap, exactap, weakap, bogusap);

  mps_ap_destroy(bogusap);
  mps_ap_destroy(weakap);
  mps_ap_destroy(exactap);
  mps_ap_destroy(leafap);
  mps_pool_destroy(tablepool);
  mps_pool_destroy(leafpool);
  mps_fmt_destroy(dylanweakfmt);
  mps_fmt_destroy(dylanfmt);
  mps_root_destroy(stack);
}


int main(int argc, char *argv[])
{
  struct guff_s guff;
  mps_arena_t arena;
  mps_thr_t thread;

  testlib_init(argc, argv);

  initialise_wrapper(wrapper_wrapper);
  initialise_wrapper(string_wrapper);
  initialise_wrapper(table_wrapper);

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "arena_create\n");
  die(mps_thread_reg(&thread, arena), "thread_reg");
  guff.arena = arena;
  guff.thr = thread;
  setup(&guff);
  mps_thread_dereg(thread);
  mps_arena_destroy(arena);

  printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
