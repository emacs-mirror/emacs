/* impl.c.amcsshe: POOL CLASS AMC STRESS TEST WITH HEADER
 *
 * $HopeName: MMsrc!amcsshe.c(trunk.4) $
 * Copyright (C) 2001 Harlequin Limited.  All rights reserved.
 */

#include "fmthe.h"
#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "mpstd.h"
#ifdef MPS_OS_W3
#include "mpsw3.h"
#endif
#include "mps.h"
#include <stdlib.h>
#include <string.h>


/* These values have been tuned to cause one top-generation collection. */
#define testArenaSIZE     ((size_t)1000*1024)
#define avLEN             3
#define exactRootsCOUNT   200
#define ambigRootsCOUNT   50
#define bogusRootsCOUNT   4096
#define collectionsCOUNT  37
#define rampSIZE          9
#define initTestFREQ      6000
#define genCOUNT          2

/* testChain -- generation parameters for the test */

static mps_gen_param_s testChain[genCOUNT] = {
  { 150, 0.85 }, { 170, 0.45 } };


/* objNULL needs to be odd so that it's ignored in exactRoots. */
#define objNULL           ((mps_addr_t)0xDECEA5ED)


static mps_pool_t pool;
static mps_ap_t ap;
static mps_addr_t exactRoots[exactRootsCOUNT];
static mps_addr_t ambigRoots[ambigRootsCOUNT];
static mps_addr_t bogusRoots[bogusRootsCOUNT];



static mps_word_t *ww = NULL;
static mps_word_t *tvw;


static mps_word_t dylan_make_WV(mps_word_t version, mps_word_t vb,
                                mps_word_t es, mps_word_t vf) 
{
  /* VERSION- ... VB------ reserved ES---VF- */
  return((version << (MPS_WORD_WIDTH - 8)) |
	 (vb << 16) |
	 (es << 3) |
	 vf);
}


static mps_res_t init(mps_addr_t addr, size_t size,
                      mps_addr_t *refs, size_t nr_refs)
{

  /* Make sure the size is aligned. */
  if ((size & (ALIGN-1)) != 0) return MPS_RES_PARAM;

  if (ww == NULL) {
    ww = malloc(sizeof(mps_word_t) * (BASIC_WRAPPER_SIZE + 1));
    if (ww == NULL) return MPS_RES_MEMORY;
    tvw = malloc(sizeof(mps_word_t) * BASIC_WRAPPER_SIZE);
    if (tvw == NULL) {
      free(ww);
      return MPS_RES_MEMORY;
    }

    /* Build a wrapper wrapper. */
    ww[WW] = (mps_word_t)ww;
    ww[WC] = (mps_word_t)ww;     /* dummy class */
    ww[WM] = (1 << 2) | 1;       /* dummy subtype_mask */
    ww[WF] = ((WS - 1) << 2) | 2;
    ww[WV] = dylan_make_WV(2, 0, 0, 0);
    ww[WS] = (1 << 2) | 1;
    ww[WP] = 1;

    /* Build a wrapper for traceable vectors. */
    tvw[WW] = (mps_word_t)ww;
    tvw[WC] = (mps_word_t)ww;    /* dummy class */
    tvw[WM] = (1 << 2) | 1;      /* dummy subtype_mask */
    tvw[WF] = 0;                 /* no fixed part */
    tvw[WV] = dylan_make_WV(2, 0, 0, 2); /* traceable variable part */
    tvw[WS] = 1;                 /* no patterns */
  }

  /* If there is enough room, make a vector, otherwise just */
  /* make a padding object. */

  if (size >= sizeof(mps_word_t) * 2) {
    mps_word_t *p = (mps_word_t *)addr;
    mps_word_t i, t = (size / sizeof(mps_word_t)) - 2;

    p[0] = (mps_word_t)tvw;     /* install vector wrapper */
    p[1] = (t << 2) | 1;        /* tag the vector length */
    for(i = 0; i < t; ++i) {
      mps_word_t r = rnd();

      if (r & 1)
        p[2+i] = ((r & ~(mps_word_t)3) | 1); /* random int */
      else
        p[2+i] = (mps_word_t)refs[(r >> 1) % nr_refs]; /* random ptr */
    }
  } else {
    die(MPS_RES_FAIL, "small object");
  }

  return MPS_RES_OK;
}


static void dylan_write(mps_addr_t addr, mps_addr_t *refs, size_t nr_refs)
{
  mps_word_t *p = (mps_word_t *)addr;
  mps_word_t t = p[1] >> 2;

  /* If the object is a vector, update a random entry. */
  if (p[0] == (mps_word_t)tvw && t > 0) {
    mps_word_t r = rnd();
    size_t i = 2 + (rnd() % t);

    if (r & 1)
      p[i] = ((r & ~(mps_word_t)3) | 1); /* random int */
    else
      p[i] = (mps_word_t)refs[(r >> 1) % nr_refs]; /* random ptr */
  }
}


static mps_addr_t make(void)
{
  size_t length = rnd() % (2*avLEN);
  size_t size = (length+2) * sizeof(mps_word_t);
  mps_addr_t p, userP;
  mps_res_t res;

  do {
    MPS_RESERVE_BLOCK(res, p, ap, size + headerSIZE);
    if (res)
      die(res, "MPS_RESERVE_BLOCK");
    userP = (mps_addr_t)((char*)p + headerSIZE);
    res = init(userP, size, exactRoots, exactRootsCOUNT);
    if (res)
      die(res, "dylan_init");
    ((int*)p)[0] = realTYPE;
    ((int*)p)[1] = 0xED0ED;
  } while(!mps_commit(ap, p, size + headerSIZE));

  return userP;
}


/* test -- the body of the test */

static void *test(void *arg, size_t s)
{
  mps_arena_t arena;
  mps_fmt_t format;
  mps_chain_t chain;
  mps_root_t exactRoot, ambigRoot, bogusRoot;
  unsigned long objs; size_t i;
  mps_word_t collections, rampSwitch;
  mps_alloc_pattern_t ramp = mps_alloc_pattern_ramp();
  int ramping;
  mps_ap_t busy_ap;
  mps_addr_t busy_init;

  arena = (mps_arena_t)arg;
  (void)s; /* unused */

  die(EnsureHeaderFormat(&format, arena), "fmt_create");
  die(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

  die(mps_pool_create(&pool, arena, mps_class_amc(), format, chain),
      "pool_create(amc)");

  die(mps_ap_create(&ap, pool, MPS_RANK_EXACT), "BufferCreate");
  die(mps_ap_create(&busy_ap, pool, MPS_RANK_EXACT), "BufferCreate 2");

  for(i = 0; i < exactRootsCOUNT; ++i)
    exactRoots[i] = objNULL;
  for(i = 0; i < ambigRootsCOUNT; ++i)
    ambigRoots[i] = (mps_addr_t)rnd();

  die(mps_root_create_table_masked(&exactRoot, arena,
                                   MPS_RANK_EXACT, (mps_rm_t)0,
                                   &exactRoots[0], exactRootsCOUNT,
                                   (mps_word_t)1),
      "root_create_table(exact)");
  die(mps_root_create_table(&ambigRoot, arena,
                            MPS_RANK_AMBIG, (mps_rm_t)0,
                            &ambigRoots[0], ambigRootsCOUNT),
      "root_create_table(ambig)");
  die(mps_root_create_table(&bogusRoot, arena,
                            MPS_RANK_AMBIG, (mps_rm_t)0,
                            &bogusRoots[0], bogusRootsCOUNT),
      "root_create_table(bogus)");

  /* create an ap, and leave it busy */
  die(mps_reserve(&busy_init, busy_ap, 64), "mps_reserve busy");

  collections = 0;
  rampSwitch = rampSIZE;
  mps_ap_alloc_pattern_begin(ap, ramp);
  mps_ap_alloc_pattern_begin(busy_ap, ramp);
  ramping = 1;
  objs = 0;
  while(collections < collectionsCOUNT) {
    unsigned long c;
    size_t r;

    c = mps_collections(arena);

    if (collections != c) {
      collections = c;
      printf("\nCollection %lu, %lu objects.\n", c, objs);
      for(r = 0; r < exactRootsCOUNT; ++r) {
        if (exactRoots[r] != objNULL)
          die(HeaderFormatCheck(exactRoots[r]), "wrapper check");
      }
      if (collections == rampSwitch) {
        rampSwitch += rampSIZE;
        if (ramping) {
          mps_ap_alloc_pattern_end(ap, ramp);
          mps_ap_alloc_pattern_end(busy_ap, ramp);
          /* kill half of the roots */
          for(i = 0; i < exactRootsCOUNT; i += 2) {
            if (exactRoots[i] != objNULL) {
              die(HeaderFormatCheck(exactRoots[i]), "ramp kill check");
              exactRoots[i] = objNULL;
            }
          }
          /* Every other time, switch back immediately. */
          if (collections & 1) ramping = 0;
        }
        if (!ramping) {
          mps_ap_alloc_pattern_begin(ap, ramp);
          mps_ap_alloc_pattern_begin(busy_ap, ramp);
          ramping = 1;
        }
      }
      /*  fill bogusRoots with variations of a real pointer */
      r = rnd() % exactRootsCOUNT;
      if (exactRoots[i] != objNULL) {
        char *p = (char*)exactRoots[i];

        for(i = 0; i < bogusRootsCOUNT; ++i, ++p)
          bogusRoots[i] = (mps_addr_t)p;
      }
    }

    r = (size_t)rnd();
    if (r & 1) {
      i = (r >> 1) % exactRootsCOUNT;
      if (exactRoots[i] != objNULL)
        die(HeaderFormatCheck(exactRoots[i]), "wrapper check");
      exactRoots[i] = make();
      if (exactRoots[(exactRootsCOUNT-1) - i] != objNULL)
        dylan_write(exactRoots[(exactRootsCOUNT-1) - i],
                    exactRoots, exactRootsCOUNT);
    } else {
      i = (r >> 1) % ambigRootsCOUNT;
      ambigRoots[(ambigRootsCOUNT-1) - i] = make();
      /* Create random interior pointers */
      ambigRoots[i] = (mps_addr_t)((char *)(ambigRoots[i/2]) + 1);
    }

    if (r % initTestFREQ == 0)
      *(int*)busy_init = -1; /* check that the buffer is still there */

    if (objs % 1024 == 0) {
      putchar('.');
      fflush(stdout);
    }

    ++objs;
  }

  (void)mps_commit(busy_ap, busy_init, 64);
  mps_ap_destroy(busy_ap);
  mps_ap_destroy(ap);
  mps_root_destroy(exactRoot);
  mps_root_destroy(ambigRoot);
  mps_root_destroy(bogusRoot);
  mps_pool_destroy(pool);
  mps_chain_destroy(chain);
  mps_fmt_destroy(format);

  return NULL;
}


int main(int argc, char **argv)
{
  mps_arena_t arena;
  mps_thr_t thread;
  void *r;

  randomize(argc, argv);

  die(mps_arena_create(&arena, mps_arena_class_vm(), 3*testArenaSIZE),
      "arena_create\n");
  die(mps_arena_commit_limit_set(arena, testArenaSIZE), "set limit");
  die(mps_thread_reg(&thread, arena), "thread_reg");
  mps_tramp(&r, test, arena, 0);
  mps_thread_dereg(thread);
  mps_arena_destroy(arena);

  fflush(stdout); /* synchronize */
  fprintf(stderr, "\nConclusion:  Failed to find any defects.\n");
  return 0;
}
