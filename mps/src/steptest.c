/* impl.c.steptest: TEST FOR ARENA CLAMPING AND STEPPING
 *
 * $HopeName: !amcss.c(trunk.31) $
 * Copyright (C) 1998 Harlequin Limited.  All rights reserved.
 *
 * Based on impl.c.amcss.
 */

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


#define testArenaSIZE     ((size_t)64<<20)
#define avLEN             3
#define exactRootsCOUNT   300
#define ambigRootsCOUNT   50
#define objCOUNT          1000000
/* objNULL needs to be odd so that it's ignored in exactRoots. */
#define objNULL           ((mps_addr_t)0xDECEA5ED)

static mps_pool_t pool;
static mps_ap_t ap;
static mps_addr_t exactRoots[exactRootsCOUNT];
static mps_addr_t ambigRoots[ambigRootsCOUNT];

#ifdef MPS_OS_W3

static HANDLE currentProcess;

static void prepare_clock(void)
{
  me = GetCurrentProcess();
}

static double my_clock(void)
{
    HANDLE me;
    FILETIME ctime, etime, ktime, utime;
    double dk, du;
    me = GetCurrentProcess();
    GetProcessTimes(me, &ctime, &etime, &ktime, &utime);
    dk = ktime.dwHighDateTime * 4096.0 * 1024.0 * 1024.0 + ktime.dwLowDateTime;
    dk /= 10.0;
    du = utime.dwHighDateTime * 4096.0 * 1024.0 * 1024.0 + utime.dwLowDateTime;
    du /= 10.0;
    return (du+dk);
}

#else
/* on Posix systems, we can use getrusage. */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>

static void prepare_clock(void)
{
}

static double myclock(void)
{
    struct rusage ru;
    getrusage(RUSAGE_SELF, &ru);
    return ((ru.ru_utime.tv_sec +
             ru.ru_stime.tv_sec) * 1000000.0 +
            (ru.ru_utime.tv_usec) +
            (ru.ru_stime.tv_usec));
}
#endif

double alloc_time, step_time, no_step_time, max_step_time, max_no_step_time, max_alloc_time;

long steps, no_steps;

long alloc_bytes;
long commit_failures;

#define CLOCK_TESTS 100000

static double clock_timing(void)
{
    long i;
    double t1, t2;

    t2 = 0.0;
    for (i=0; i<CLOCK_TESTS; ++i) {
        t1 = myclock();
        t2 += myclock()-t1;
    }
    return t2/CLOCK_TESTS;
}

static mps_addr_t make(void)
{
  size_t length = rnd() % (2*avLEN);
  size_t size = (length+2) * sizeof(mps_word_t);
  mps_addr_t p;
  mps_res_t res;

  alloc_bytes += size;

  for(;;) {
      int commit_res;
      double t1 = myclock();
      MPS_RESERVE_BLOCK(res, p, ap, size);
      t1 = myclock() - t1;
      alloc_time += t1;
      if (t1 > max_alloc_time)
          max_alloc_time = t1;
      if(res)
          die(res, "MPS_RESERVE_BLOCK");
      res = dylan_init(p, size, exactRoots, exactRootsCOUNT);
      if(res)
          die(res, "dylan_init");
      t1 = myclock();
      commit_res = mps_commit(ap,p,size);
      t1 = myclock() - t1;
      alloc_time += t1;
      if (t1 > max_alloc_time)
          max_alloc_time = t1;
      if (commit_res)
          break;
      else
          ++ commit_failures;
  }

  return p;
}


static void test_step(mps_arena_t arena)
{
  mps_bool_t res;
  double t1 = myclock();
  res = mps_arena_step(arena, 0.1);
  t1 = myclock() - t1;
  if (res) {
      if (t1 > max_step_time)
          max_step_time = t1;
      step_time += t1;
      ++ steps;
  } else {
      if (t1 > max_no_step_time)
          max_no_step_time = t1;
      no_step_time += t1;
      ++ no_steps;
  }
}


static void *test(void *arg, size_t s)
{
  mps_arena_t arena;
  mps_fmt_t format;
  mps_root_t exactRoot, ambigRoot;
  unsigned long objs; size_t i;
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

  objs = 0;
  steps = no_steps = 0;
  alloc_bytes = 0;
  commit_failures = 0;
  alloc_time = step_time = no_step_time = 0.0;
  max_alloc_time = max_step_time = max_no_step_time = 0.0;

  while(objs < objCOUNT) {
    size_t r;

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

    if(objs % 1000 == 0)
      test_step(arena);

    ++objs;
  }

  printf("%ld objects (%ld bytes) allocated\n", objs, alloc_bytes);
  printf("commit failed %ld times\n", commit_failures);
  printf("allocation took %.0f us, mean %.2f us, max %.0f us\n",
         alloc_time, alloc_time / objs, max_alloc_time);
  printf("%ld steps took %.0f us, mean %.2f us, max %.0f us\n",
         steps, step_time, step_time / steps, max_step_time);
  printf("%ld non-steps took %.0f us, mean %.2f us, max %.0f us\n",
         no_steps, no_step_time, no_step_time / no_steps, max_no_step_time);

  printf("clock timing %.2f us\n", clock_timing());

  (void)mps_commit(busy_ap, busy_init, 64);
  mps_ap_destroy(busy_ap);
  mps_ap_destroy(ap);
  mps_root_destroy(exactRoot);
  mps_root_destroy(ambigRoot);
  mps_pool_destroy(pool);
  mps_fmt_destroy(format);

  return NULL;
}


int main(int argc, char **argv)
{
  mps_arena_t arena;
  mps_thr_t thread;
  void *r;

  prepare_clock();

  randomize(argc, argv);

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "arena_create\n");
  adjust_collection_freq(0.2);
  die(mps_thread_reg(&thread, arena), "thread_reg");
  mps_tramp(&r, test, arena, 0);
  mps_thread_dereg(thread);
  mps_arena_destroy(arena);

  fflush(stdout); /* synchronize */
  fprintf(stderr, "\nConclusion:  Failed to find any defects.\n");
  return 0;
}
