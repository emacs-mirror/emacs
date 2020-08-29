/* amcssth.c: POOL CLASS AMC STRESS TEST WITH TWO THREADS
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (c) 2002 Global Graphics Software.
 *
 * The main thread parks the arena half way through the test case and
 * runs mps_arena_formatted_objects_walk(). This checks that walking
 * works while the other threads continue to allocate in the
 * background.
 */

#include "fmtdy.h"
#include "fmtdytst.h"
#include "testlib.h"
#include "testthr.h"
#include "mpslib.h"
#include "mpscamc.h"
#include "mpsavm.h"

#include <stdio.h> /* fflush, printf, putchar */


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


static mps_word_t collections;
static mps_arena_t arena;
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
  mps_thr_t thread1, thread2;
  mps_root_t reg_root;
  mps_ap_t ap;
  closure_t cl = arg;

  /* Register the thread twice to check this is supported -- see
   * <design/thread-manager#.req.register.multi>
   */
  die(mps_thread_reg(&thread1, arena), "thread_reg");
  die(mps_thread_reg(&thread2, arena), "thread_reg");
  die(mps_root_create_thread(&reg_root, arena, thread1, marker),
      "root_create");

  die(mps_ap_create(&ap, cl->pool, mps_rank_exact()), "BufferCreate(fooey)");
  while(mps_collections(arena) < collectionsCOUNT) {
    churn(ap, cl->roots_count);
  }
  mps_ap_destroy(ap);

  mps_root_destroy(reg_root);
  mps_thread_dereg(thread2);
  mps_thread_dereg(thread1);

  return NULL;
}


/* test -- the body of the test */

static void test_pool(const char *name, mps_pool_t pool, size_t roots_count)
{
  size_t i;
  mps_word_t rampSwitch;
  mps_alloc_pattern_t ramp = mps_alloc_pattern_ramp();
  int ramping;
  mps_ap_t ap, busy_ap;
  mps_addr_t busy_init;
  testthr_t kids[10];
  closure_s cl;
  int walked = FALSE, ramped = FALSE;

  printf("\n------ pool: %s-------\n", name);

  cl.pool = pool;
  cl.roots_count = roots_count;
  collections = 0;

  for (i = 0; i < NELEMS(kids); ++i)
    testthr_create(&kids[i], kid_thread, &cl);

  die(mps_ap_create(&ap, pool, mps_rank_exact()), "BufferCreate");
  die(mps_ap_create(&busy_ap, pool, mps_rank_exact()), "BufferCreate 2");

  /* create an ap, and leave it busy */
  die(mps_reserve(&busy_init, busy_ap, 64), "mps_reserve busy");

  rampSwitch = rampSIZE;
  die(mps_ap_alloc_pattern_begin(ap, ramp), "pattern begin (ap)");
  die(mps_ap_alloc_pattern_begin(busy_ap, ramp), "pattern begin (busy_ap)");
  ramping = 1;
  while (collections < collectionsCOUNT) {
    mps_message_type_t type;

    if (mps_message_queue_type(&type, arena)) {
      mps_message_t msg;
      mps_bool_t b = mps_message_get(&msg, arena, type);
      Insist(b); /* we just checked there was one */

      if (type == mps_message_type_gc()) {
        size_t live = mps_message_gc_live_size(arena, msg);
        size_t condemned = mps_message_gc_condemned_size(arena, msg);
        size_t not_condemned = mps_message_gc_not_condemned_size(arena, msg);

        printf("\nCollection %lu finished:\n", (unsigned long)collections++);
        printf("live %"PRIuLONGEST"\n", (ulongest_t)live);
        printf("condemned %"PRIuLONGEST"\n", (ulongest_t)condemned);
        printf("not_condemned %"PRIuLONGEST"\n", (ulongest_t)not_condemned);

      } else if (type == mps_message_type_gc_start()) {
        printf("\nCollection %lu started, %lu objects, committed=%lu.\n",
               (unsigned long)collections, objs,
               (unsigned long)mps_arena_committed(arena));

        for (i = 0; i < exactRootsCOUNT; ++i)
          cdie(exactRoots[i] == objNULL || dylan_check(exactRoots[i]),
               "all roots check");

        if (collections >= collectionsCOUNT / 2 && !walked)
        {
          unsigned long count = 0;
          mps_arena_park(arena);
          mps_arena_formatted_objects_walk(arena, test_stepper, &count, 0);
          mps_arena_release(arena);
          printf("stepped on %lu objects.\n", count);
          walked = TRUE;
        }
        if (collections >= rampSwitch && !ramped) {
          /* Every other time, switch back immediately. */
          int begin_ramp = !ramping || (collections & 1);

          rampSwitch += rampSIZE;
          if (ramping) {
            die(mps_ap_alloc_pattern_end(ap, ramp), "pattern end (ap)");
            die(mps_ap_alloc_pattern_end(busy_ap, ramp),
                "pattern end (busy_ap)");
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

      mps_message_discard(arena, msg);
    }

    churn(ap, roots_count);
    {
      size_t r = (size_t)rnd();
      if (r % initTestFREQ == 0)
        *(int*)busy_init = -1; /* check that the buffer is still there */
    }
    if (objs % 1024 == 0) {
      putchar('.');
      fflush(stdout);
    }
  }

  (void)mps_commit(busy_ap, busy_init, 64);
  mps_ap_destroy(busy_ap);
  mps_ap_destroy(ap);

  for (i = 0; i < NELEMS(kids); ++i)
    testthr_join(&kids[i], NULL);
}

static void test_arena(void)
{
  size_t i;
  mps_fmt_t format;
  mps_chain_t chain;
  mps_thr_t thread;
  mps_root_t reg_root;
  mps_pool_t amc_pool, amcz_pool;
  void *marker = &marker;

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, testArenaSIZE);
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_GRAIN_SIZE, rnd_grain(testArenaSIZE));
    die(mps_arena_create_k(&arena, mps_arena_class_vm(), args), "arena_create");
  } MPS_ARGS_END(args);
  mps_message_type_enable(arena, mps_message_type_gc());
  mps_message_type_enable(arena, mps_message_type_gc_start());

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
  die(mps_thread_reg(&thread, arena), "thread_reg");
  die(mps_root_create_thread(&reg_root, arena, thread, marker),
      "root_create");

  die(mps_pool_create(&amc_pool, arena, mps_class_amc(), format, chain),
      "pool_create(amc)");
  die(mps_pool_create(&amcz_pool, arena, mps_class_amcz(), format, chain),
      "pool_create(amcz)");

  test_pool("AMC", amc_pool, exactRootsCOUNT);
  test_pool("AMCZ", amcz_pool, 0);

  mps_arena_park(arena);
  mps_pool_destroy(amc_pool);
  mps_pool_destroy(amcz_pool);
  mps_root_destroy(reg_root);
  mps_thread_dereg(thread);
  mps_root_destroy(exactRoot);
  mps_root_destroy(ambigRoot);
  mps_chain_destroy(chain);
  mps_fmt_destroy(format);
  mps_arena_destroy(arena);
}

int main(int argc, char *argv[])
{
  testlib_init(argc, argv);
  test_arena();

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
