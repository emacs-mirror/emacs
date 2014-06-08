/* amcsshe.c: POOL CLASS AMC STRESS TEST WITH HEADER
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (c) 2002 Global Graphics Software.
 */

#include "fmthe.h"
#include "fmtdytst.h"
#include "testlib.h"
#include "mpslib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "mpstd.h"
#include "mps.h"

#include <stdio.h> /* fflush, printf, putchar */


/* These values have been tuned in the hope of getting one dynamic collection. */
#define headerFACTOR      ((float)(20 + headerSIZE) / 20)
/* headerFACTOR measures how much larger objects are compared to fmtdy. */
#define testArenaSIZE     ((size_t)(1000*headerFACTOR)*1024)
#define gen1SIZE          ((size_t)(150*headerFACTOR))
#define gen2SIZE          ((size_t)(170*headerFACTOR))
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
  { gen1SIZE, 0.85 }, { gen2SIZE, 0.45 } };


/* objNULL needs to be odd so that it's ignored in exactRoots. */
#define objNULL           ((mps_addr_t)MPS_WORD_CONST(0xDECEA5ED))


static mps_pool_t pool;
static mps_ap_t ap;
static mps_addr_t exactRoots[exactRootsCOUNT];
static mps_addr_t ambigRoots[ambigRootsCOUNT];
static mps_addr_t bogusRoots[bogusRootsCOUNT];

static mps_addr_t make(size_t roots_count)
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
    res = dylan_init(userP, size, exactRoots, roots_count);
    if (res)
      die(res, "dylan_init");
    ((int*)p)[0] = realHeader;
    ((int*)p)[1] = 0xED0ED;
  } while(!mps_commit(ap, p, size + headerSIZE));

  return userP;
}


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


/* test -- the body of the test */

static void *test(mps_arena_t arena, mps_class_t pool_class, size_t roots_count)
{
  mps_fmt_t format;
  mps_chain_t chain;
  mps_root_t exactRoot, ambigRoot, bogusRoot;
  unsigned long objs; size_t i;
  mps_word_t collections, rampSwitch;
  mps_alloc_pattern_t ramp = mps_alloc_pattern_ramp();
  int ramping;
  mps_ap_t busy_ap;
  mps_addr_t busy_init;

  die(EnsureHeaderFormat(&format, arena), "fmt_create");
  die(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

  die(mps_pool_create(&pool, arena, pool_class, format, chain),
      "pool_create(amc)");

  die(mps_ap_create(&ap, pool, mps_rank_exact()), "BufferCreate");
  die(mps_ap_create(&busy_ap, pool, mps_rank_exact()), "BufferCreate 2");

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
  die(mps_root_create_table(&bogusRoot, arena,
                            mps_rank_ambig(), (mps_rm_t)0,
                            &bogusRoots[0], bogusRootsCOUNT),
      "root_create_table(bogus)");

  /* create an ap, and leave it busy */
  die(mps_reserve(&busy_init, busy_ap, 64), "mps_reserve busy");

  collections = 0;
  rampSwitch = rampSIZE;
  die(mps_ap_alloc_pattern_begin(ap, ramp), "pattern begin (ap)");
  die(mps_ap_alloc_pattern_begin(busy_ap, ramp), "pattern begin (busy_ap)");
  ramping = 1;
  objs = 0;
  while (collections < collectionsCOUNT) {
    mps_word_t c;
    size_t r;

    c = mps_collections(arena);

    if (collections != c) {
      collections = c;
      printf("\nCollection %"PRIuLONGEST", %lu objects.\n",
             (ulongest_t)c, objs);
      report(arena);
      for (r = 0; r < exactRootsCOUNT; ++r) {
        if (exactRoots[r] != objNULL)
          die(HeaderFormatCheck(exactRoots[r]), "wrapper check");
      }
      if (collections == rampSwitch) {
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
              die(HeaderFormatCheck(exactRoots[i]), "ramp kill check");
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
      /*  fill bogusRoots with variations of a real pointer */
      r = rnd() % exactRootsCOUNT;
      if (exactRoots[r] != objNULL) {
        char *p = (char*)exactRoots[r];

        for(i = 0; i < bogusRootsCOUNT; ++i, ++p)
          bogusRoots[i] = (mps_addr_t)p;
      }
    }

    r = (size_t)rnd();
    if (r & 1) {
      i = (r >> 1) % exactRootsCOUNT;
      if (exactRoots[i] != objNULL)
        die(HeaderFormatCheck(exactRoots[i]), "wrapper check");
      exactRoots[i] = make(roots_count);
      if (exactRoots[(exactRootsCOUNT-1) - i] != objNULL)
        dylan_write(exactRoots[(exactRootsCOUNT-1) - i],
                    exactRoots, exactRootsCOUNT);
    } else {
      i = (r >> 1) % ambigRootsCOUNT;
      ambigRoots[(ambigRootsCOUNT-1) - i] = make(roots_count);
      /* Create random interior pointers */
      ambigRoots[i] = (mps_addr_t)((char *)(ambigRoots[i/2]) + 1);
    }

    if (r % initTestFREQ == 0)
      *(int*)busy_init = -1; /* check that the buffer is still there */

    if (objs % 1024 == 0) {
      report(arena);
      putchar('.');
      (void)fflush(stdout);
    }

    ++objs;
  }

  (void)mps_commit(busy_ap, busy_init, 64);
  mps_arena_park(arena);
  mps_ap_destroy(busy_ap);
  mps_ap_destroy(ap);
  mps_root_destroy(exactRoot);
  mps_root_destroy(ambigRoot);
  mps_root_destroy(bogusRoot);
  mps_pool_destroy(pool);
  mps_chain_destroy(chain);
  mps_fmt_destroy(format);
  mps_arena_release(arena);

  return NULL;
}


int main(int argc, char *argv[])
{
  mps_arena_t arena;
  mps_thr_t thread;

  testlib_init(argc, argv);

  die(mps_arena_create(&arena, mps_arena_class_vm(), 2*testArenaSIZE),
      "arena_create\n");
  mps_message_type_enable(arena, mps_message_type_gc());
  die(mps_arena_commit_limit_set(arena, 2*testArenaSIZE), "set limit");
  die(mps_thread_reg(&thread, arena), "thread_reg");
  test(arena, mps_class_amc(), exactRootsCOUNT);
  test(arena, mps_class_amcz(), 0);
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
