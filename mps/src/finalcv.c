/* impl.c.finalcv: FINALIZATION COVERAGE TEST
 *
 * $HopeName: MMsrc!finalcv.c(trunk.1) $
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
 *
 * .hack.no-interface: There is no MPS interface to PoolMRG, and there
 * is no MPS interface to finalization.  For the moment this test uses
 * PoolMRG directly.  Later it will use the MPS interface to
 * finalization.
 */


#include "testlib.h"
#include "mps.h"
#include "mpscamc.h"
#include "fmtdy.h"

#include "poolmrg.h" /* .hack.no-interface */

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>


#define N_ROOTS 20
#define CHURN_FACTOR 1000
#define ONE_SLOT_SIZE (3*sizeof(mps_word_t))
#define MAGIC_0 36	/* source is person.richard */
/* The number that a half of all numbers generated from rnd are less
 * than.  Hence, probability a-half, or P a-half */
/* see impl.h.testlib */
#define P_A_HALF (1024uL*1024uL*1024uL - 1)	/* 2^30 - 1 */

/* tags an integer according to dylan format */
static mps_word_t dylan_int(mps_word_t x)
{
  return (x << 2)|1;
}

/* .hack.no-interface */
static mps_class_t mps_class_mrg(void)
{
  return (mps_class_t)PoolClassMRG();
}

static void *root[N_ROOTS];
static int rc = 0;			/* return code */

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
  int i;			/* index */
  mps_ap_t ap;
  mps_fmt_t fmt;
  mps_pool_t amc;
  mps_pool_t mrg;
  mps_res_t e;
  mps_root_t mps_root[2];
  mps_space_t space;
  void *mrg_obj[N_ROOTS];
  void *p = NULL;

  space = (mps_space_t)arg;

  die(mps_fmt_create_A(&fmt, space, dylan_fmt_A()), "fmt_create\n");
  die(mps_pool_create(&amc, space, mps_class_amc(), fmt),
      "pool_create amc\n");
  die(mps_pool_create(&mrg, space, mps_class_mrg()), /* .hack.no-interface */
      "pool_create mrg\n");
  die(mps_root_create_table(&mps_root[0], space,
			     MPS_RANK_EXACT, (mps_rm_t)0,
			     root, (size_t)N_ROOTS), "root_create\n");
  die(mps_root_create_table(&mps_root[1], space,
			     MPS_RANK_EXACT, (mps_rm_t)0,
			     &p, (size_t)1), "root_create\n");
  die(mps_ap_create(&ap, amc, MPS_RANK_EXACT), "ap_create\n");

  /* design.mps.poolmrg.test.alloc */
  for(i = 0; i < N_ROOTS; ++i) {
    e = mps_alloc(&mrg_obj[i], mrg, sizeof(mps_addr_t));
    assert(e == MPS_RES_OK);
  }
  /* design.mps.poolmrg.test.free */
  for(i = 0; i < N_ROOTS; ++i) {
    mps_free(mrg, mrg_obj[i], sizeof(mps_addr_t));
  }

  /* design.mps.poolmrg.test.rw.a */
  do {
    MPS_RESERVE_BLOCK(e, p, ap, ONE_SLOT_SIZE);
    assert(e == MPS_RES_OK);
    dylan_init(p, ONE_SLOT_SIZE, root, 1);
  } while(!mps_commit(ap, p, ONE_SLOT_SIZE));

  /* design.mps.poolmrg.test.rw.alloc */
  for(i = 0; i < N_ROOTS; ++i) {
    e = mps_alloc(&mrg_obj[i], mrg, sizeof(mps_addr_t));
    assert(e == MPS_RES_OK);
    /* design.mps.poolmrg.test.rw.write */
    *(mps_addr_t *)mrg_obj[i] = p;
  }
  /* design.mps.poolmrg.test.rw.read */
  for(i = 0; i < N_ROOTS; ++i) {
    if(*(mps_addr_t *)mrg_obj[i] != p) {
      printf("Couldn't correctly read reference in guardian object.\n");
      ++rc;
    }
  }
  /* design.mps.poolmrg.test.rw.free */
  for(i = 0; i < N_ROOTS; ++i) {
    mps_free(mrg, mrg_obj[i], sizeof(mps_addr_t));
  }
  /* design.mps.poolmrg.test.rw.drop */
  p = NULL;

  /* design.mps.poolmrg.test.fl.alloc */
  for(i = 0; i < N_ROOTS; ++i) {
    do {
      MPS_RESERVE_BLOCK(e, p, ap, ONE_SLOT_SIZE);
      assert(e == MPS_RES_OK);
      dylan_init(p, ONE_SLOT_SIZE, root, 1);
    } while(!mps_commit(ap, p, ONE_SLOT_SIZE));
    root[i] = p;
    /* design.mps.poolmrg.test.fl.tag */
    ((mps_word_t *)p)[2] = dylan_int(MAGIC_0 ^ i);
    e = mps_alloc(&mrg_obj[i], mrg, sizeof(mps_addr_t));
    assert(e == MPS_RES_OK);
    /* design.mps.poolmrg.test.fl.refer */
    *(mps_addr_t *)mrg_obj[i] = p;
  }
  p = NULL;

  /* design.mps.poolmrg.test.fl.churn */
  churn(ap);
  for(i = 0; i < N_ROOTS; ++i) {
    mps_word_t *o;
    o = *(mps_addr_t *)mrg_obj[i];
    if(o[2] != dylan_int(MAGIC_0 ^ i)) {
      printf("Reference in guardian object has been corrupted or "
             "referent has been corrupted.\n");
      ++rc;
    }
  }
  for(i = 0; i < N_ROOTS; ++i) {
    mps_free(mrg, mrg_obj[i], sizeof(mps_addr_t));
  }

  /* design.mps.poolmrg.test.ut.alloc */
  for(i = 0; i < N_ROOTS; ++i) {
    do {
      MPS_RESERVE_BLOCK(e, p, ap, ONE_SLOT_SIZE);
      assert(e == MPS_RES_OK);
      dylan_init(p, ONE_SLOT_SIZE, root, 1);
    } while(!mps_commit(ap, p, ONE_SLOT_SIZE));
    e = mps_alloc(&mrg_obj[i], mrg, sizeof(mps_addr_t));
    assert(e == MPS_RES_OK);
    /* design.mps.poolmrg.test.ut.refer */
    *(mps_addr_t *)mrg_obj[i] = p;
    root[i] = p;
  }
  p = NULL;

  /* design.mps.poolmrg.test.ut.drop */
  for(i = 0; i < N_ROOTS; ++i) {
    if(rnd() < P_A_HALF) {
      root[i] = NULL;
    }
  }
  /* design.mps.poolmrg.test.ut.churn */
  churn(ap);

  /* design.mps.poolmrg.test.ut.not */

  mps_ap_destroy(ap);
  mps_root_destroy(mps_root[0]);
  mps_root_destroy(mps_root[1]);
  mps_pool_destroy(amc);
  mps_pool_destroy(mrg);
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
