/* impl.c.amcss: POOL CLASS AMC STRESS TEST
 *
 * $HopeName: !amcssth.c(trunk.1) $
 * Copyright (C) 2000 Harlequin Ltd.  All rights reserved.
 */

#define _POSIX_C_SOURCE 199309L

#include "fmtdy.h"
#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "mpstd.h"
#ifdef MPS_OS_W3
#include "mpsw3.h"
#endif
#include "mps.h"
#ifdef MPS_OS_SU
#include "ossu.h"
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <pthread.h>
#include <time.h>


#define testArenaSIZE     ((size_t)64<<20)
#define avLEN             3
#define exactRootsCOUNT   300
#define ambigRootsCOUNT   50
#define collectionsCOUNT  18
#define rampSIZE          5
#define initTestFREQ      6000
/* objNULL needs to be odd so that it's ignored in exactRoots. */
#define objNULL           ((mps_addr_t)0xDECEA5ED)

static mps_pool_t pool;
static mps_ap_t ap;
static mps_addr_t exactRoots[exactRootsCOUNT];
static mps_addr_t ambigRoots[ambigRootsCOUNT];


static mps_addr_t make(void)
{
  size_t length = rnd() % (2*avLEN);
  size_t size = (length+2) * sizeof(mps_word_t);
  mps_addr_t p;
  mps_res_t res;

  do {
    MPS_RESERVE_BLOCK(res, p, ap, size);
    if(res)
      die(res, "MPS_RESERVE_BLOCK");
    res = dylan_init(p, size, exactRoots, exactRootsCOUNT);
    if(res)
      die(res, "dylan_init");
  } while(!mps_commit(ap, p, size));

  return p;
}


static void test_stepper(mps_addr_t object, void *p, size_t s)
{
  (*(unsigned long *)p)++;
  testlib_unused(s);
  testlib_unused(object);
}


static void *test(void *arg, size_t s)
{
  mps_arena_t arena;
  mps_fmt_t format;
  mps_root_t exactRoot, ambigRoot;
  unsigned long objs; size_t i;
  mps_word_t collections, rampSwitch;
  mps_alloc_pattern_t ramp = mps_alloc_pattern_ramp();
  int ramping;
  mps_ap_t busy_ap;
  mps_addr_t busy_init;

  arena = (mps_arena_t)arg;
  (void)s; /* unused */

  die(dylan_fmt(&format, arena), "fmt_create");

  die(mps_pool_create(&pool, arena, mps_class_amc(), format),
      "pool_create(amc)");

  die(mps_ap_create(&ap, pool, MPS_RANK_EXACT), "BufferCreate");
  die(mps_ap_create(&busy_ap, pool, MPS_RANK_EXACT), "BufferCreate");

  for(i=0; i<exactRootsCOUNT; ++i)
    exactRoots[i] = objNULL;
  for(i=0; i<ambigRootsCOUNT; ++i)
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

    if(collections != c) {
      collections = c;
      printf("\nCollection %lu, %lu objects.\n",
             c, objs);
      for(r = 0; r < exactRootsCOUNT; ++r)
        assert(exactRoots[r] == objNULL ||
               dylan_check(exactRoots[r]));
      if(collections == collectionsCOUNT / 2) {
        unsigned long object_count = 0;
        mps_arena_park(arena);
	mps_amc_apply(pool, test_stepper, &object_count, 0);
	mps_arena_release(arena);
	printf("mps_amc_apply stepped on %lu objects.\n", object_count);
      }
      if(collections == rampSwitch) {
        rampSwitch += rampSIZE;
        if(ramping) {
          mps_ap_alloc_pattern_end(ap, ramp);
          mps_ap_alloc_pattern_end(busy_ap, ramp);
          /* Every other time, switch back immediately. */
          if(collections & 1) ramping = 0;
        }
        if(!ramping) {
          mps_ap_alloc_pattern_begin(ap, ramp);
          mps_ap_alloc_pattern_begin(busy_ap, ramp);
          ramping = 1;
        }
      }
    }

    r = rnd();
    if(r & 1) {
      i = (r >> 1) % exactRootsCOUNT;
      if(exactRoots[i] != objNULL)
        assert(dylan_check(exactRoots[i]));
      exactRoots[i] = make();
      if(exactRoots[(exactRootsCOUNT-1) - i] != objNULL)
        dylan_write(exactRoots[(exactRootsCOUNT-1) - i],
                    exactRoots, exactRootsCOUNT);
    } else {
      i = (r >> 1) % ambigRootsCOUNT;
      ambigRoots[(ambigRootsCOUNT-1) - i] = make();
      /* Create random interior pointers */
      ambigRoots[i] = (mps_addr_t)((char *)(ambigRoots[i/2]) + 1);
    }

    if(r % initTestFREQ == 0)
      *(int*)busy_init = -1; /* check that the buffer is still there */

    if(objs % 1024 == 0) {
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
  mps_pool_destroy(pool);
  mps_fmt_destroy(format);

  return NULL;
}


static void *fooey2(void *arg, size_t s)
{
  #if 0
  struct timeval now;
  int wake_sec;
  gettimeofday(&now, NULL);
  wake_sec = now.tv_sec + 20;
  do {
    sleep(1);
    gettimeofday(&now, NULL);
  } while (wake_sec > now.tv_sec);
  #endif

  struct timespec req, rem;
  req.tv_sec = 10;
  req.tv_nsec = 0;
  while(nanosleep(&req, &rem))
    req = rem;
  return arg;
}

static void *fooey(void *arena)
{
  void *r;
  mps_thr_t thread;
  mps_thr_t thread2;
  /* register the thread twice, just to make sure it works */
  die(mps_thread_reg(&thread, (mps_arena_t)arena), "thread_reg");
  die(mps_thread_reg(&thread2, (mps_arena_t)arena), "thread2_reg");
  mps_tramp(&r, fooey2, (mps_arena_t)arena, 0);
  mps_thread_dereg(thread);
  mps_thread_dereg(thread2);
  return r;
}

int main(void)
{
  mps_arena_t arena;
  mps_thr_t thread;
  pthread_t pthread1;
  void *r;

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "arena_create\n");
  adjust_collection_freq(0.0);
  die(mps_thread_reg(&thread, arena), "thread_reg");
  pthread_create(&pthread1, NULL, fooey, (void *)arena);
  mps_tramp(&r, test, arena, 0);
  mps_thread_dereg(thread);
  mps_arena_destroy(arena);

  fflush(stdout); /* synchronize */
  fprintf(stderr, "\nConclusion:  Failed to find any defects.\n");
  return 0;
}
