/* steptest.c: TEST FOR ARENA CLAMPING AND STEPPING
 *
 * $Id$
 * Copyright (C) 1998 Ravenbrook Limited.  See end of file for license.
 *
 * Based on <code/amcss.c>.
 */

#include "fmtdy.h"
#include "fmtdytst.h"
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

#define testArenaSIZE     ((size_t)(64l << 20))
#define avLEN             3
#define exactRootsCOUNT   200
#define ambigRootsCOUNT   50
#define objCOUNT          1000000
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


#ifdef MPS_OS_W3

static HANDLE currentProcess;

static void prepare_clock(void)
{
  currentProcess = GetCurrentProcess();
}

static double my_clock(void)
{
    FILETIME ctime, etime, ktime, utime;
    double dk, du;
    GetProcessTimes(currentProcess, &ctime, &etime, &ktime, &utime);
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

static double my_clock(void)
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
        t1 = my_clock();
        t2 += my_clock()-t1;
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
    mps_bool_t commit_res;
    double t1 = my_clock();
    MPS_RESERVE_BLOCK(res, p, ap, size);
    t1 = my_clock() - t1;
    alloc_time += t1;
    if (t1 > max_alloc_time)
      max_alloc_time = t1;
    if(res)
      die(res, "MPS_RESERVE_BLOCK");
    res = dylan_init(p, size, exactRoots, exactRootsCOUNT);
    if(res)
      die(res, "dylan_init");
    t1 = my_clock();
    commit_res = mps_commit(ap, p, size);
    t1 = my_clock() - t1;
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
  double t1 = my_clock();
  res = mps_arena_step(arena, 0.1);
  t1 = my_clock() - t1;
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

/* test -- the body of the test */

static void *test(void *arg, size_t s)
{
  mps_arena_t arena;
  mps_fmt_t format;
  mps_chain_t chain;
  mps_root_t exactRoot, ambigRoot;
  unsigned long objs; size_t i;

  arena = (mps_arena_t)arg;
  (void)s; /* unused */

  die(dylan_fmt(&format, arena), "fmt_create");
  die(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

  die(mps_pool_create(&pool, arena, mps_class_amc(), format, chain),
      "pool_create(amc)");

  die(mps_ap_create(&ap, pool, MPS_RANK_EXACT), "BufferCreate");

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

  objs = 0;
  steps = no_steps = 0;
  alloc_bytes = 0;
  commit_failures = 0;
  alloc_time = step_time = no_step_time = 0.0;
  max_alloc_time = max_step_time = max_no_step_time = 0.0;

  while(objs < objCOUNT) {
    size_t r;

    r = (size_t)rnd();
    if(r & 1) {
      i = (r >> 1) % exactRootsCOUNT;
      if(exactRoots[i] != objNULL)
        cdie(dylan_check(exactRoots[i]), "dying root check");
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

    if(objs % 1000 == 0) {
        test_step(arena);
    }

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

  mps_ap_destroy(ap);
  mps_root_destroy(exactRoot);
  mps_root_destroy(ambigRoot);
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

  prepare_clock();

  randomize(argc, argv);

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "arena_create");
  die(mps_arena_commit_limit_set(arena, testArenaSIZE), "set limit");
  die(mps_thread_reg(&thread, arena), "thread_reg");
  mps_tramp(&r, test, arena, 0);
  mps_thread_dereg(thread);
  mps_arena_destroy(arena);

  fflush(stdout); /* synchronize */
  fprintf(stderr, "\nConclusion:  Failed to find any defects.\n");
  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
