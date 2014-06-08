/* amsss.c: POOL CLASS AMS STRESS TEST
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (c) 2002 Global Graphics Software.
 *
 * .design: Adapted from amcss.c, but not counting collections, just
 * total size of objects allocated (because epoch doesn't increment when
 * AMS is collected).  */

#include "fmtdy.h"
#include "fmtdytst.h"
#include "testlib.h"
#include "mpslib.h"
#include "mpscams.h"
#include "mpsavm.h"
#include "mpstd.h"
#include "mps.h"

#include <stdio.h> /* fflush, printf */


#define exactRootsCOUNT 50
#define ambigRootsCOUNT 100
/* This is enough for three GCs. */
#define totalSizeMAX    800 * (size_t)1024
#define totalSizeSTEP   200 * (size_t)1024
/* objNULL needs to be odd so that it's ignored in exactRoots. */
#define objNULL         ((mps_addr_t)MPS_WORD_CONST(0xDECEA5ED))
#define testArenaSIZE   ((size_t)16<<20)
#define initTestFREQ    3000
#define splatTestFREQ   6000
static mps_gen_param_s testChain[1] = { { 160, 0.90 } };


static mps_arena_t arena;
static mps_ap_t ap;
static mps_addr_t exactRoots[exactRootsCOUNT];
static mps_addr_t ambigRoots[ambigRootsCOUNT];
static size_t totalSize = 0;


/* report - report statistics from any messages */

static void report(void)
{
  static int nStart = 0;
  static int nComplete = 0;
  mps_message_type_t type;

  while(mps_message_queue_type(&type, arena)) {
    mps_message_t message;

    cdie(mps_message_get(&message, arena, type), "message get");

    if (type == mps_message_type_gc_start()) {
      printf("\nCollection start %d.  Because:\n", ++nStart);
      printf("%s\n", mps_message_gc_start_why(arena, message));

    } else if (type == mps_message_type_gc()) {
      size_t live, condemned, not_condemned;

      live = mps_message_gc_live_size(arena, message);
      condemned = mps_message_gc_condemned_size(arena, message);
      not_condemned = mps_message_gc_not_condemned_size(arena, message);

      printf("\nCollection complete %d:\n", ++nComplete);
      printf("live %"PRIuLONGEST"\n", (ulongest_t)live);
      printf("condemned %"PRIuLONGEST"\n", (ulongest_t)condemned);
      printf("not_condemned %"PRIuLONGEST"\n", (ulongest_t)not_condemned);

    } else {
      cdie(0, "unknown message type");
    }

    mps_message_discard(arena, message);
  }

  return;
}


/* make -- object allocation and init */

static mps_addr_t make(void)
{
  size_t length = rnd() % 20, size = (length+2) * sizeof(mps_word_t);
  mps_addr_t p;
  mps_res_t res;

  do {
    MPS_RESERVE_BLOCK(res, p, ap, size);
    if (res)
      die(res, "MPS_RESERVE_BLOCK");
    res = dylan_init(p, size, exactRoots, exactRootsCOUNT);
    if (res)
      die(res, "dylan_init");
  } while(!mps_commit(ap, p, size));

  totalSize += size;
  return p;
}


/* test -- the actual stress test */

static mps_pool_debug_option_s freecheckOptions =
  { NULL, 0, "Dead", 4 };

static void *test(void *arg, size_t haveAmbigous)
{
  mps_pool_t pool;
  mps_root_t exactRoot, ambigRoot = NULL;
  size_t lastStep = 0, i, r;
  unsigned long objs;
  mps_ap_t busy_ap;
  mps_addr_t busy_init;

  pool = (mps_pool_t)arg;

  die(mps_ap_create(&ap, pool, mps_rank_exact()), "BufferCreate");
  die(mps_ap_create(&busy_ap, pool, mps_rank_exact()), "BufferCreate 2");

  for(i = 0; i < exactRootsCOUNT; ++i)
    exactRoots[i] = objNULL;
  if (haveAmbigous)
    for(i = 0; i < ambigRootsCOUNT; ++i)
      ambigRoots[i] = rnd_addr();

  die(mps_root_create_table_masked(&exactRoot, arena,
                                   mps_rank_exact(), (mps_rm_t)0,
                                   &exactRoots[0], exactRootsCOUNT,
                                   (mps_word_t)1),
      "root_create_table(exact)");
  if (haveAmbigous)
    die(mps_root_create_table(&ambigRoot, arena,
                              mps_rank_ambig(), (mps_rm_t)0,
                              &ambigRoots[0], ambigRootsCOUNT),
        "root_create_table(ambig)");

  /* create an ap, and leave it busy */
  die(mps_reserve(&busy_init, busy_ap, 64), "mps_reserve busy");

  objs = 0; totalSize = 0;
  while(totalSize < totalSizeMAX) {
    if (totalSize > lastStep + totalSizeSTEP) {
      lastStep = totalSize;
      printf("\nSize %"PRIuLONGEST" bytes, %lu objects.\n",
             (ulongest_t)totalSize, objs);
      (void)fflush(stdout);
      for(i = 0; i < exactRootsCOUNT; ++i)
        cdie(exactRoots[i] == objNULL || dylan_check(exactRoots[i]),
             "all roots check");
    }

    r = (size_t)rnd();
    if (!haveAmbigous || (r & 1)) {
      i = (r >> 1) % exactRootsCOUNT;
      if (exactRoots[i] != objNULL)
        cdie(dylan_check(exactRoots[i]), "dying root check");
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

    if (rnd() % initTestFREQ == 0)
      *(int*)busy_init = -1; /* check that the buffer is still there */

    if (rnd() % splatTestFREQ == 0)
      mps_pool_check_free_space(pool);

    ++objs;
    if (objs % 256 == 0) {
      printf(".");
      report();
      (void)fflush(stdout);
    }
  }

  (void)mps_commit(busy_ap, busy_init, 64);
  mps_ap_destroy(busy_ap);
  mps_ap_destroy(ap);
  mps_root_destroy(exactRoot);
  if (haveAmbigous)
    mps_root_destroy(ambigRoot);

  return NULL;
}


int main(int argc, char *argv[])
{
  mps_thr_t thread;
  mps_fmt_t format;
  mps_chain_t chain;
  mps_pool_t pool;
  void *r;

  testlib_init(argc, argv);

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "arena_create");
  mps_message_type_enable(arena, mps_message_type_gc_start());
  mps_message_type_enable(arena, mps_message_type_gc());
  die(mps_thread_reg(&thread, arena), "thread_reg");
  die(mps_fmt_create_A(&format, arena, dylan_fmt_A()), "fmt_create");
  die(mps_chain_create(&chain, arena, 1, testChain), "chain_create");

  /* TODO: Add tests using the arena default chain. */

  printf("\n\n****************************** Testing AMS Debug\n");
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_CHAIN, chain);
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format);
    MPS_ARGS_ADD(args, MPS_KEY_AMS_SUPPORT_AMBIGUOUS, FALSE);
    MPS_ARGS_ADD(args, MPS_KEY_POOL_DEBUG_OPTIONS, &freecheckOptions);
    die(mps_pool_create_k(&pool, arena, mps_class_ams_debug(), args),
        "pool_create(ams_debug,share)");
  } MPS_ARGS_END(args);
  mps_tramp(&r, test, pool, 0);
  mps_pool_destroy(pool);

  printf("\n\n****************************** Testing AMS Debug\n");
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_CHAIN, chain);
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format);
    MPS_ARGS_ADD(args, MPS_KEY_AMS_SUPPORT_AMBIGUOUS, TRUE);
    MPS_ARGS_ADD(args, MPS_KEY_POOL_DEBUG_OPTIONS, &freecheckOptions);
    die(mps_pool_create_k(&pool, arena, mps_class_ams_debug(), args),
        "pool_create(ams_debug,ambig)");
  } MPS_ARGS_END(args);
  mps_tramp(&r, test, pool, 1);
  mps_pool_destroy(pool);

  printf("\n\n****************************** Testing AMS\n");
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_CHAIN, chain);
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format);
    MPS_ARGS_ADD(args, MPS_KEY_AMS_SUPPORT_AMBIGUOUS, TRUE);
    MPS_ARGS_ADD(args, MPS_KEY_POOL_DEBUG_OPTIONS, &freecheckOptions);
    die(mps_pool_create_k(&pool, arena, mps_class_ams(), args),
        "pool_create(ams,ambig)");
  } MPS_ARGS_END(args);
  mps_tramp(&r, test, pool, 1);
  mps_pool_destroy(pool);

  printf("\n\n****************************** Testing AMS\n");
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_CHAIN, chain);
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format);
    MPS_ARGS_ADD(args, MPS_KEY_AMS_SUPPORT_AMBIGUOUS, FALSE);
    MPS_ARGS_ADD(args, MPS_KEY_POOL_DEBUG_OPTIONS, &freecheckOptions);
    die(mps_pool_create_k(&pool, arena, mps_class_ams(), args),
        "pool_create(ams,share)");
  } MPS_ARGS_END(args);
  mps_tramp(&r, test, pool, 0);
  mps_pool_destroy(pool);

  mps_chain_destroy(chain);
  mps_fmt_destroy(format);
  mps_thread_dereg(thread);
  mps_arena_destroy(arena);

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
