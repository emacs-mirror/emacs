/* impl.c.finalcv: FINALIZATION COVERAGE TEST
 *
 * $HopeName: MMsrc!finalcv.c(trunk.5) $
 * Copyright (C) 1996,1997 Harlequin Group, all rights reserved
 *
 * READERSHIP
 *
 * Any MPS developer; Any interested QA
 *
 * DESIGN
 *
 * See design.mps.poolmrg.test.
 *
 * PURPOSE
 *
 * To test finalization in the MPS.
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

/* What does the next line mean? @@@@ */
/* .hack.order.1, .hack.order.2 */

#include "testlib.h"
#include "mps.h"
#include "mpscamc.h"
#include "fmtdy.h"

#ifdef MPS_OS_SU
#include "ossu.h"
#endif

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>


#define N_ROOTS 20
#define CHURN_FACTOR 1000
#define ONE_SLOT_SIZE (3*sizeof(mps_word_t))
/* The number that a half of all numbers generated from rnd are less
 * than.  Hence, probability a-half, or P a-half */
/* see impl.h.testlib */
#define P_A_HALF (1024uL*1024uL*1024uL - 1)     /* 2^30 - 1 */

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

static void *root[N_ROOTS];
static int rc = 0;                      /* return code */

static void
churn(mps_ap_t ap)
{
  int i;
  mps_addr_t p;
  mps_res_t e;
  
  for(i = 0; i < CHURN_FACTOR; ++i) {
    do {
      MPS_RESERVE_BLOCK(e, p, ap, 4096);
      assert(e == MPS_RES_OK);
      dylan_init(p, 4096, root, 1);
    } while(!mps_commit(ap, p, 4096));
  }
  p = NULL;
}


static void *
test(void *arg, size_t s)
{
  int i;                        /* index */
  mps_ap_t ap;
  mps_fmt_t fmt;
  mps_pool_t amc;
  mps_res_t e;
  mps_root_t mps_root[2];
  mps_space_t space;
  void *p = NULL;
  mps_message_t message;
  space = (mps_space_t)arg;

  die(mps_fmt_create_A(&fmt, space, dylan_fmt_A()), "fmt_create\n");
  die(mps_pool_create(&amc, space, mps_class_amc(), fmt),
      "pool_create amc\n");
  die(mps_root_create_table(&mps_root[0], space,
                            MPS_RANK_EXACT, (mps_rm_t)0,
                            root, (size_t)N_ROOTS), "root_create\n");
  die(mps_root_create_table(&mps_root[1], space,
                            MPS_RANK_EXACT, (mps_rm_t)0,
                            &p, (size_t)1), "root_create\n");
  die(mps_ap_create(&ap, amc, MPS_RANK_EXACT), "ap_create\n");


  /* design.mps.poolmrg.test.ut.alloc */
  for(i = 0; i < N_ROOTS; ++i) {
    do {
      MPS_RESERVE_BLOCK(e, p, ap, ONE_SLOT_SIZE);
      assert(e == MPS_RES_OK);
      dylan_init(p, ONE_SLOT_SIZE, root, 1);
    } while(!mps_commit(ap, p, ONE_SLOT_SIZE));
    ((mps_word_t *)p)[2] = dylan_int(i);
    die(mps_finalize(space, p), "finalize\n");
    root[i] = p;
  }
  p = NULL;

  /* design.mps.poolmrg.test.ut.drop */
  for(i = 0; i < N_ROOTS; ++i) {
    if(rnd() < P_A_HALF) {
      root[i] = NULL;
    }
  }

  mps_message_type_enable(space, mps_message_type_finalization());

  /* design.mps.poolmrg.test.ut.churn */
  while(mps_collections(space) < 3) {
    churn(ap);
    while(mps_message_poll(space)) {
      int b;
      mps_word_t *obj;
      mps_word_t objind;
      mps_addr_t objaddr;

      b = mps_message_get(&message, space, mps_message_type_finalization());
      assert(b);
      mps_message_finalization_ref(&objaddr, space, message);
      obj = objaddr;
      objind = dylan_int_int(obj[2]);
      printf("Finalizing: object %lu at %p\n", objind, objaddr);
      assert(root[objind] == NULL);
      root[objind] = objaddr;
    }
  }

  /* design.mps.poolmrg.test.ut.not */

  mps_ap_destroy(ap);
  mps_root_destroy(mps_root[1]);
  mps_root_destroy(mps_root[0]);
  mps_pool_destroy(amc);
  mps_fmt_destroy(fmt);

  return NULL;
}

int
main(void)
{
  mps_space_t space;
  mps_thr_t thread;
  void *r;

  die(mps_space_create(&space), "space_create\n");
  die(mps_thread_reg(&thread, space), "thread_reg\n");
  mps_tramp(&r, test, space, 0);
  mps_thread_dereg(thread);
  mps_space_destroy(space);

  if(rc) {
    printf("Defects found, exiting with non-zero status.\n");
  } else {
    printf("No defects found.\n");
  }

  return rc;
}
