/* impl.c.finalcv: FINALIZATION COVERAGE TEST
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 * Copyright (C) 2002 Global Graphics Software.
 *
 * DESIGN
 *
 * See design.mps.poolmrg.test.
 *
 * DEPENDENCIES
 *
 * This test uses the dylan object format, but the reliance on this
 * particular format is not great and could be removed.
 *
 * NOTES
 *
 * This code was created by first copying impl.c.weakcv
 */

#include "testlib.h"
#include "mps.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "fmtdy.h"
#include "fmtdytst.h"
#include "mpstd.h"
#ifdef MPS_OS_W3
#include "mpsw3.h"
#endif
#include <stdlib.h>


#define testArenaSIZE   ((size_t)16<<20)
#define rootCOUNT 20
#define churnFACTOR 10
#define finalizationRATE 6
#define gcINTERVAL ((size_t)150 * 1024)
#define collectionCOUNT 3
#define slotSIZE (3*sizeof(mps_word_t))
#define genCOUNT 2

/* testChain -- generation parameters for the test */

static mps_gen_param_s testChain[genCOUNT] = {
  { 150, 0.85 }, { 170, 0.45 } };


/* tags an integer according to dylan format */
static mps_word_t dylan_int(mps_word_t x)
{
  return (x << 2)|1;
}


/* converts a dylan format int to an int (untags) */
static mps_word_t dylan_int_int(mps_word_t x)
{
  return x >> 2;
}


static void *root[rootCOUNT];


static void churn(mps_ap_t ap)
{
  int i;
  mps_addr_t p;
  mps_res_t e;

  for (i = 0; i < churnFACTOR; ++i) {
    do {
      MPS_RESERVE_BLOCK(e, p, ap, 4096);
      die(e, "MPS_RESERVE_BLOCK");
      die(dylan_init(p, 4096, root, 1), "dylan_init");
    } while (!mps_commit(ap, p, 4096));
  }
  p = NULL;
}


enum {
  rootSTATE,
  deadSTATE,
  finalizableSTATE,
  finalizedSTATE
};


static void *test(void *arg, size_t s)
{
  int i;                        /* index */
  mps_ap_t ap;
  mps_fmt_t fmt;
  mps_chain_t chain;
  mps_pool_t amc;
  mps_res_t e;
  mps_root_t mps_root[2];
  int state[rootCOUNT];
  mps_arena_t arena;
  void *p = NULL;
#ifdef CONFIG_PROD_EPCORE
  size_t gcThreshold;
#endif
  mps_message_t message;

  arena = (mps_arena_t)arg;
  (void)s;

  die(mps_fmt_create_A(&fmt, arena, dylan_fmt_A()), "fmt_create\n");
  die(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");
  die(mps_pool_create(&amc, arena, mps_class_amc(), fmt, chain),
      "pool_create amc\n");
  die(mps_root_create_table(&mps_root[0], arena, MPS_RANK_EXACT, (mps_rm_t)0,
                            root, (size_t)rootCOUNT),
      "root_create\n");
  die(mps_root_create_table(&mps_root[1], arena, MPS_RANK_EXACT, (mps_rm_t)0,
                            &p, (size_t)1),
      "root_create\n");
  die(mps_ap_create(&ap, amc, MPS_RANK_EXACT), "ap_create\n");

  /* design.mps.poolmrg.test.promise.ut.alloc */
  for (i = 0; i < rootCOUNT; ++i) {
    do {
      MPS_RESERVE_BLOCK(e, p, ap, slotSIZE);
      die(e, "MPS_RES_OK");
      die(dylan_init(p, slotSIZE, root, 1), "dylan_init");
    } while (!mps_commit(ap, p, slotSIZE));
    ((mps_word_t *)p)[2] = dylan_int(i);
    die(mps_finalize(arena, &p), "finalize\n");
    root[i] = p; state[i] = rootSTATE;
  }
  p = NULL;

  mps_message_type_enable(arena, mps_message_type_finalization());

#ifdef CONFIG_PROD_EPCORE
  gcThreshold = mps_arena_committed(arena) + gcINTERVAL;
#endif
  /* design.mps.poolmrg.test.promise.ut.churn */
  while (mps_collections(arena) < collectionCOUNT) {
    churn(ap);
    /* design.mps.poolmrg.test.promise.ut.drop */
    for (i = 0; i < rootCOUNT; ++i) {
      if (root[i] != NULL && state[i] == rootSTATE) {
        if (rnd() % finalizationRATE == 0) {
          /* definalize some of them */
          if (rnd() % 2 == 0) {
            die(mps_definalize(arena, &root[i]), "definalize\n");
            state[i] = deadSTATE;
          } else {
            state[i] = finalizableSTATE;
          }
          root[i] = NULL;
        }
      }
    }
#ifdef CONFIG_PROD_EPCORE
    if (mps_arena_committed(arena) > gcThreshold) {
      die(mps_arena_collect(arena), "collect");
      gcThreshold = mps_arena_committed(arena) + gcINTERVAL;
    }
#endif
    while (mps_message_poll(arena)) {
      mps_word_t *obj;
      mps_word_t objind;
      mps_addr_t objaddr;

      /* design.mps.poolmrg.test.promise.ut.message */
      cdie(mps_message_get(&message, arena, mps_message_type_finalization()),
           "get");
      mps_message_finalization_ref(&objaddr, arena, message);
      obj = objaddr;
      objind = dylan_int_int(obj[2]);
      printf("Finalizing: object %lu at %p\n", objind, objaddr);
      /* design.mps.poolmrg.test.promise.ut.final.check */
      cdie(root[objind] == NULL, "finalized live");
      cdie(state[objind] == finalizableSTATE, "finalized dead");
      state[objind] = finalizedSTATE;
      /* sometimes resurrect */
      if (rnd() % 2 == 0)
        root[objind] = objaddr;
      mps_message_discard(arena, message);
    }
  }

  /* @@@@ design.mps.poolmrg.test.promise.ut.nofinal.check missing */

  mps_ap_destroy(ap);
  mps_root_destroy(mps_root[1]);
  mps_root_destroy(mps_root[0]);
  mps_pool_destroy(amc);
  mps_chain_destroy(chain);
  mps_fmt_destroy(fmt);

  return NULL;
}


int main(int argc, char **argv)
{
  mps_arena_t arena;
  mps_thr_t thread;
  void *r;

  randomize(argc, argv);

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "arena_create\n");
  die(mps_thread_reg(&thread, arena), "thread_reg\n");
  mps_tramp(&r, test, arena, 0);
  mps_thread_dereg(thread);
  mps_arena_destroy(arena);

  fflush(stdout); /* synchronize */
  fprintf(stderr, "\nConclusion:  Failed to find any defects.\n");
  return 0;
}
