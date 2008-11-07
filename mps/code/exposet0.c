/* exposet0.c: ARENA EXPOSE TEST
 *
 * $Id$
 * Copyright (c) 2001,2003 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 * 
 * The primary purpose of this test is to test that mps_arena_expose does
 * not protect any pages.  This is only tested to any real extent on
 * Windows where an exception handler in test_stepper is used to catch any
 * would-be exceptions (there aren't any if the MPS is operating
 * correctly).
 * 
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
#define objNULL           ((mps_addr_t)0xDECEA5ED)


static mps_pool_t pool_g;
static mps_ap_t ap;
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
    printf("live %lu\n", (unsigned long)live);
    printf("condemned %lu\n", (unsigned long)condemned);
    printf("not_condemned %lu\n", (unsigned long)not_condemned);

    mps_message_discard(arena, message);

    if (condemned > (gen1SIZE + gen2SIZE + (size_t)128) * 1024)
      /* When condemned size is larger than could happen in a gen 2
       * collection (discounting ramps, natch), guess that was a dynamic
       * collection, and reset the commit limit, so it doesn't run out. */
      die(mps_arena_commit_limit_set(arena, 2 * testArenaSIZE), "set limit");
  }
}


/* make -- create one new object */

static mps_addr_t make(void)
{
  size_t length = rnd() % (2*avLEN);
  size_t size = (length+2) * sizeof(mps_word_t);
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

  return p;
}


/* test_stepper -- stepping function for walk */

static void test_stepper(mps_addr_t object, mps_fmt_t fmt, mps_pool_t pool,
                         void *p, size_t s)
{
  testlib_unused(fmt);
  testlib_unused(pool);
  testlib_unused(s);
#ifdef MPS_OS_W3
  __try {
    dylan_mutate(object);
  } __except(EXCEPTION_EXECUTE_HANDLER) {
    error("Unexpected exception.\n");
  }
#else
  dylan_mutate(object);
#endif
      
  (*(unsigned long *)p)++;
}


/* test -- the body of the test */

static void *test(void *arg, size_t s)
{
  mps_addr_t busy_init;
  mps_ap_t busy_ap;
  mps_arena_t arena;
  mps_chain_t chain;
  mps_fmt_t format;
  mps_root_t exactRoot, ambigRoot;
  mps_word_t collections;
  size_t i;
  unsigned long objs;

  arena = (mps_arena_t)arg;
  (void)s; /* unused */

  die(dylan_fmt(&format, arena), "fmt_create");
  die(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

  die(mps_pool_create(&pool_g, arena, mps_class_amc(), format, chain),
      "pool_create(amc)");

  die(mps_ap_create(&ap, pool_g, MPS_RANK_EXACT), "BufferCreate");
  die(mps_ap_create(&busy_ap, pool_g, MPS_RANK_EXACT), "BufferCreate 2");

  for(i = 0; i < exactRootsCOUNT; ++i) {
    exactRoots[i] = objNULL;
  }
  for(i = 0; i < ambigRootsCOUNT; ++i) {
    ambigRoots[i] = rnd_addr();
  }

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
  objs = 0;
  while (collections < collectionsCOUNT) {
    unsigned long c;
    size_t r;

    c = mps_collections(arena);

    if (collections != c) {
      collections = c;
      printf("\nCollection %lu started, %lu objects.\n", c, objs);

      report(arena);
      for (i = 0; i < exactRootsCOUNT; ++i) {
        cdie(exactRoots[i] == objNULL
             || (dylan_check(exactRoots[i])
                 && mps_arena_has_addr(arena, exactRoots[i])),
             "all roots check");
      }
      cdie(!mps_arena_has_addr(arena, NULL),
           "NULL in arena");

      {
        unsigned long object_count = 0;
        mps_arena_expose(arena);
        mps_arena_formatted_objects_walk(arena, test_stepper, &object_count, 0);
        mps_arena_release(arena);
        printf("stepped on %lu objects.\n", object_count);
      }
    }

    r = (size_t)rnd();
    if (r & 1) {
      i = (r >> 1) % exactRootsCOUNT;
      if (exactRoots[i] != objNULL) {
        cdie(dylan_check(exactRoots[i]), "dying root check");
      }
      exactRoots[i] = make();
      if (exactRoots[(exactRootsCOUNT-1) - i] != objNULL) {
        dylan_write(exactRoots[(exactRootsCOUNT-1) - i],
                    exactRoots, exactRootsCOUNT);
      }
    } else {
      i = (r >> 1) % ambigRootsCOUNT;
      ambigRoots[(ambigRootsCOUNT-1) - i] = make();
      /* Create random interior pointers */
      ambigRoots[i] = (mps_addr_t)((char *)(ambigRoots[i/2]) + 1);
    }

    if (r % initTestFREQ == 0) {
      *(int*)busy_init = -1; /* check that the buffer is still there */
    }

    if (objs % 1024 == 0) {
      report(arena);
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
  mps_pool_destroy(pool_g);
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

  die(mps_arena_create(&arena, mps_arena_class_vm(), 2*testArenaSIZE),
      "arena_create");
  mps_message_type_enable(arena, mps_message_type_gc());
  die(mps_arena_commit_limit_set(arena, testArenaSIZE), "set limit");
  die(mps_thread_reg(&thread, arena), "thread_reg");
  mps_tramp(&r, test, arena, 0);
  mps_thread_dereg(thread);
  report(arena);
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
