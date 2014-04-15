/* amcssth.c: POOL CLASS AMC STRESS TEST WITH TWO THREADS
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (c) 2002 Global Graphics Software.
 *
 * .mode: This test case has two modes:
 *
 * .mode.walk: In this mode, the main thread parks the arena half way
 * through the test case and runs mps_arena_formatted_objects_walk().
 * This checks that walking works while the other threads continue to
 * allocate in the background.
 *
 * .mode.commit: In this mode, the arena's commit limit is set. This
 * checks that the MPS can make progress inside a tight limit in the
 * presence of allocation on multiple threads. But this is
 * incompatible with .mode.walk: if the arena is parked, then the
 * arena has no chance to make progress.
 */

#include "fmtdy.h"
#include "fmtdytst.h"
#include "testlib.h"
#include "testthr.h"
#include "mpslib.h"
#include "mpscamc.h"
#include "mpsavm.h"

#include <stdio.h> /* fflush, printf, putchar */

enum {
  ModeWALK = 0,                 /* .mode.walk */
  ModeCOMMIT = 1                /* .mode.commit */
};


/* These values have been tuned in the hope of getting one dynamic collection. */
#define testArenaSIZE     ((size_t)1000*1024)
#define gen1SIZE          ((size_t)150)
#define gen2SIZE          ((size_t)170)
#define avLEN             3
#define exactRootsCOUNT   180
#define ambigRootsCOUNT   50
#define genCOUNT          2
#define collectionsCOUNT  37
#define rampSIZE          9
#define initTestFREQ      6000

/* testChain -- generation parameters for the test */

static mps_gen_param_s testChain[genCOUNT] = {
  { gen1SIZE, 0.85 }, { gen2SIZE, 0.45 } };


/* objNULL needs to be odd so that it's ignored in exactRoots. */
#define objNULL           ((mps_addr_t)MPS_WORD_CONST(0xDECEA5ED))


static mps_addr_t exactRoots[exactRootsCOUNT];
static mps_addr_t ambigRoots[ambigRootsCOUNT];

/* report - report statistics from any terminated GCs */

static void report(mps_arena_t arena)
{
  mps_message_t message;
  static int nCollections = 0;
    
  while (mps_message_get(&message, arena, mps_message_type_gc())) {
    size_t live, condemned, not_condemned;

    live = mps_message_gc_live_size(arena, message);
    condemned = mps_message_gc_condemned_size(arena, message);
    not_condemned = mps_message_gc_not_condemned_size(arena, message);

    printf("\nCollection %d finished:\n", ++nCollections);
    printf("live %"PRIuLONGEST"\n", (ulongest_t)live);
    printf("condemned %"PRIuLONGEST"\n", (ulongest_t)condemned);
    printf("not_condemned %"PRIuLONGEST"\n", (ulongest_t)not_condemned);

    mps_message_discard(arena, message);
  }
}


static mps_arena_t arena;
static mps_fmt_t format;
static mps_chain_t chain;
static mps_root_t exactRoot, ambigRoot;
static unsigned long objs = 0;


/* make -- create one new object */

static mps_addr_t make(mps_ap_t ap, size_t roots_count)
{
  size_t length = rnd() % (2*avLEN);
  size_t size = (length+2) * sizeof(mps_word_t);
  mps_addr_t p;
  mps_res_t res;

  do {
    MPS_RESERVE_BLOCK(res, p, ap, size);
    if (res)
      die(res, "MPS_RESERVE_BLOCK");
    res = dylan_init(p, size, exactRoots, roots_count);
    if (res)
      die(res, "dylan_init");
  } while(!mps_commit(ap, p, size));

  return p;
}


/* test_stepper -- stepping function for walk */

static void test_stepper(mps_addr_t object, mps_fmt_t fmt, mps_pool_t pool,
                         void *p, size_t s)
{
  testlib_unused(object); testlib_unused(fmt); testlib_unused(pool);
  testlib_unused(s);
  (*(unsigned long *)p)++;
}


/* init -- initialize roots and chain */

static void init(void)
{
  size_t i;

  die(dylan_fmt(&format, arena), "fmt_create");
  die(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

  for(i = 0; i < exactRootsCOUNT; ++i)
    exactRoots[i] = objNULL;
  for(i = 0; i < ambigRootsCOUNT; ++i)
    ambigRoots[i] = rnd_addr();

  die(mps_root_create_table_masked(&exactRoot, arena,
                                   mps_rank_exact(), (mps_rm_t)0,
                                   &exactRoots[0], exactRootsCOUNT,
                                   (mps_word_t)1),
      "root_create_table(exact)");
  die(mps_root_create_table(&ambigRoot, arena,
                            mps_rank_ambig(), (mps_rm_t)0,
                            &ambigRoots[0], ambigRootsCOUNT),
      "root_create_table(ambig)");
}


/* finish -- finish roots and chain */

static void finish(void)
{
  mps_root_destroy(exactRoot);
  mps_root_destroy(ambigRoot);
  mps_chain_destroy(chain);
  mps_fmt_destroy(format);
}


/* churn -- create an object and install into roots */

static void churn(mps_ap_t ap, size_t roots_count)
{
  size_t i;
  size_t r;

  ++objs;
  r = (size_t)rnd();
  if (r & 1) {
    i = (r >> 1) % exactRootsCOUNT;
    if (exactRoots[i] != objNULL)
      cdie(dylan_check(exactRoots[i]), "dying root check");
    exactRoots[i] = make(ap, roots_count);
    if (exactRoots[(exactRootsCOUNT-1) - i] != objNULL)
      dylan_write(exactRoots[(exactRootsCOUNT-1) - i],
                  exactRoots, exactRootsCOUNT);
  } else {
    i = (r >> 1) % ambigRootsCOUNT;
    ambigRoots[(ambigRootsCOUNT-1) - i] = make(ap, roots_count);
    /* Create random interior pointers */
    ambigRoots[i] = (mps_addr_t)((char *)(ambigRoots[i/2]) + 1);
  }
}


typedef struct closure_s {
  mps_pool_t pool;
  size_t roots_count;
} closure_s, *closure_t;

static void *kid_thread(void *arg)
{
  void *marker = &marker;
  mps_thr_t thread;
  mps_root_t reg_root;
  mps_ap_t ap;
  closure_t cl = arg;

  die(mps_thread_reg(&thread, (mps_arena_t)arena), "thread_reg");
  die(mps_root_create_reg(&reg_root, arena, mps_rank_ambig(), 0, thread,
                          mps_stack_scan_ambig, marker, 0), "root_create");

  die(mps_ap_create(&ap, cl->pool, mps_rank_exact()), "BufferCreate(fooey)");
  while(mps_collections(arena) < collectionsCOUNT) {
    churn(ap, cl->roots_count);
  }
  mps_ap_destroy(ap);

  mps_root_destroy(reg_root);
  mps_thread_dereg(thread);

  return NULL;
}


/* test -- the body of the test */

static void *test_pool(mps_class_t pool_class, size_t roots_count, int mode)
{
  size_t i;
  mps_word_t collections, rampSwitch;
  mps_alloc_pattern_t ramp = mps_alloc_pattern_ramp();
  int ramping;
  mps_ap_t ap, busy_ap;
  mps_addr_t busy_init;
  mps_pool_t pool;
  testthr_t kids[10];
  closure_s cl;
  int walked = FALSE, ramped = FALSE;

  die(mps_pool_create(&pool, arena, pool_class, format, chain),
      "pool_create(amc)");

  cl.pool = pool;
  cl.roots_count = roots_count;

  for (i = 0; i < sizeof(kids)/sizeof(kids[0]); ++i)
    testthr_create(&kids[i], kid_thread, &cl);

  die(mps_ap_create(&ap, pool, mps_rank_exact()), "BufferCreate");
  die(mps_ap_create(&busy_ap, pool, mps_rank_exact()), "BufferCreate 2");

  /* create an ap, and leave it busy */
  die(mps_reserve(&busy_init, busy_ap, 64), "mps_reserve busy");

  collections = 0;
  rampSwitch = rampSIZE;
  die(mps_ap_alloc_pattern_begin(ap, ramp), "pattern begin (ap)");
  die(mps_ap_alloc_pattern_begin(busy_ap, ramp), "pattern begin (busy_ap)");
  ramping = 1;
  while (collections < collectionsCOUNT) {
    unsigned long c;
    size_t r;

    c = mps_collections(arena);

    if (collections != c) {
      collections = c;
      printf("\nCollection %lu started, %lu objects, committed=%lu.\n",
             c, objs, (unsigned long)mps_arena_committed(arena));
      report(arena);

      for (i = 0; i < exactRootsCOUNT; ++i)
        cdie(exactRoots[i] == objNULL || dylan_check(exactRoots[i]),
             "all roots check");

      if (mode == ModeWALK && collections >= collectionsCOUNT / 2 && !walked) {
        unsigned long object_count = 0;
        mps_arena_park(arena);
        mps_arena_formatted_objects_walk(arena, test_stepper, &object_count, 0);
        mps_arena_release(arena);
        printf("stepped on %lu objects.\n", object_count);
        walked = TRUE;
      }
      if (collections >= rampSwitch && !ramped) {
        int begin_ramp = !ramping
          || /* Every other time, switch back immediately. */ (collections & 1);

        rampSwitch += rampSIZE;
        if (ramping) {
          die(mps_ap_alloc_pattern_end(ap, ramp), "pattern end (ap)");
          die(mps_ap_alloc_pattern_end(busy_ap, ramp), "pattern end (busy_ap)");
          ramping = 0;
          /* kill half of the roots */
          for(i = 0; i < exactRootsCOUNT; i += 2) {
            if (exactRoots[i] != objNULL) {
              cdie(dylan_check(exactRoots[i]), "ramp kill check");
              exactRoots[i] = objNULL;
            }
          }
        }
        if (begin_ramp) {
          die(mps_ap_alloc_pattern_begin(ap, ramp),
              "pattern rebegin (ap)");
          die(mps_ap_alloc_pattern_begin(busy_ap, ramp),
              "pattern rebegin (busy_ap)");
          ramping = 1;
        }
      }
      ramped = TRUE;
    }

    churn(ap, roots_count);

    r = (size_t)rnd();

    if (r % initTestFREQ == 0)
      *(int*)busy_init = -1; /* check that the buffer is still there */

    if (objs % 1024 == 0) {
      report(arena);
      putchar('.');
      fflush(stdout);
    }
  }

  (void)mps_commit(busy_ap, busy_init, 64);
  mps_ap_destroy(busy_ap);
  mps_ap_destroy(ap);

  for (i = 0; i < sizeof(kids)/sizeof(kids[0]); ++i)
    testthr_join(&kids[i], NULL);

  mps_pool_destroy(pool);

  return NULL;
}

static void test_arena(int mode)
{
  mps_thr_t thread;
  mps_root_t reg_root;
  void *marker = &marker;

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "arena_create");
  if (mode == ModeCOMMIT)
    die(mps_arena_commit_limit_set(arena, 2 * testArenaSIZE), "set limit");
  mps_message_type_enable(arena, mps_message_type_gc());
  init();
  die(mps_thread_reg(&thread, arena), "thread_reg");
  die(mps_root_create_reg(&reg_root, arena, mps_rank_ambig(), 0, thread,
                          mps_stack_scan_ambig, marker, 0), "root_create");

  test_pool(mps_class_amc(), exactRootsCOUNT, mode);
  test_pool(mps_class_amcz(), 0, mode);

  mps_root_destroy(reg_root);
  mps_thread_dereg(thread);
  finish();
  report(arena);
  mps_arena_destroy(arena);
}

int main(int argc, char *argv[])
{
  testlib_init(argc, argv);
  test_arena(ModeWALK);
  test_arena(ModeCOMMIT);

  printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (c) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
