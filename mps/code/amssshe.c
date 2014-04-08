/* amssshe.c: POOL CLASS AMS STRESS TEST WITH HEADERS
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * .design: Adapted from amsss.c.
 */

#include "fmthe.h"
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
/* This is enough for five GCs. */
#define totalSizeMAX    800 * (size_t)1024
#define totalSizeSTEP   200 * (size_t)1024
/* objNULL needs to be odd so that it's ignored in exactRoots. */
#define objNULL         ((mps_addr_t)MPS_WORD_CONST(0xDECEA5ED))
#define testArenaSIZE   ((size_t)16<<20)
#define initTestFREQ    6000
static mps_gen_param_s testChain[1] = { { 160, 0.90 } };


static mps_pool_t pool;
static mps_ap_t ap;
static mps_addr_t exactRoots[exactRootsCOUNT];
static mps_addr_t ambigRoots[ambigRootsCOUNT];
static size_t totalSize = 0;


static mps_addr_t make(void)
{
  size_t length = rnd() % 20, size = (length+2) * sizeof(mps_word_t);
  mps_addr_t p, userP;
  mps_res_t res;

  do {
    MPS_RESERVE_BLOCK(res, p, ap, size + headerSIZE);
    if(res)
      die(res, "MPS_RESERVE_BLOCK");
    userP = (mps_addr_t)((char*)p + headerSIZE);
    res = dylan_init(userP, size, exactRoots, exactRootsCOUNT);
    if(res)
      die(res, "dylan_init");
    ((int*)p)[0] = realHeader;
    ((int*)p)[1] = 0xED0ED;
  } while(!mps_commit(ap, p, size + headerSIZE));

  totalSize += size;
  return userP;
}


static void *test(void *arg, size_t s)
{
  mps_arena_t arena;
  mps_fmt_t format;
  mps_chain_t chain;
  mps_root_t exactRoot, ambigRoot;
  size_t lastStep = 0, i, r;
  unsigned long objs;
  mps_ap_t busy_ap;
  mps_addr_t busy_init;

  arena = (mps_arena_t)arg;
  (void)s; /* unused */

  die(EnsureHeaderFormat(&format, arena), "make header format");
  die(mps_chain_create(&chain, arena, 1, testChain), "chain_create");
  die(mps_pool_create(&pool, arena, mps_class_ams(), format, chain,
                      TRUE), "pool_create(ams)");

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

  /* create an ap, and leave it busy */
  die(mps_reserve(&busy_init, busy_ap, 64), "mps_reserve busy");

  objs = 0;
  while(totalSize < totalSizeMAX) {
    if(totalSize > lastStep + totalSizeSTEP) {
      lastStep = totalSize;
      printf("\nSize %"PRIuLONGEST" bytes, %lu objects.\n",
             (ulongest_t)totalSize, objs);
      (void)fflush(stdout);
      for(i = 0; i < exactRootsCOUNT; ++i)
        cdie(exactRoots[i] == objNULL || dylan_check(exactRoots[i]),
             "all roots check");
    }

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

    if(rnd() % initTestFREQ == 0)
      *(int*)busy_init = -1; /* check that the buffer is still there */

    ++objs;
    if (objs % 256 == 0) {
      printf(".");
      (void)fflush(stdout);
    }
  }

  (void)mps_commit(busy_ap, busy_init, 64);
  mps_ap_destroy(busy_ap);
  mps_ap_destroy(ap);
  mps_root_destroy(exactRoot);
  mps_root_destroy(ambigRoot);
  mps_pool_destroy(pool);
  mps_chain_destroy(chain);
  mps_fmt_destroy(format);

  return NULL;
}


int main(int argc, char *argv[])
{
  mps_arena_t arena;
  mps_thr_t thread;
  void *r;

  testlib_init(argc, argv);

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "arena_create");
  die(mps_thread_reg(&thread, arena), "thread_reg");
  mps_tramp(&r, test, arena, 0);
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
