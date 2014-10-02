/* finalcv.c: FINALIZATION COVERAGE TEST
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * DESIGN
 *
 * See <design/poolmrg/#test>.
 *
 * DEPENDENCIES
 *
 * This test uses the dylan object format, but the reliance on this
 * particular format is not great and could be removed.
 *
 * NOTES
 *
 * This code was created by first copying <code/weakcv.c>
 */

#include "fmtdy.h"
#include "fmtdytst.h"
#include "mpm.h"
#include "mps.h"
#include "mpsavm.h"
#include "mpscamc.h"
#include "mpscams.h"
#include "mpscawl.h"
#include "mpsclo.h"
#include "mpslib.h"
#include "mpstd.h"
#include "testlib.h"

#include <stdio.h> /* printf */


#define testArenaSIZE   ((size_t)16<<20)
#define rootCOUNT 20
#define churnFACTOR 10
#define finalizationRATE 6
#define gcINTERVAL ((size_t)150 * 1024)
#define messageCOUNT 3

/* 3 words:  wrapper  |  vector-len  |  first-slot */
#define vectorSIZE (3*sizeof(mps_word_t))
#define vectorSLOT 2

#define genCOUNT 2

/* testChain -- generation parameters for the test */

static mps_gen_param_s testChain[genCOUNT] = {
  { 150, 0.85 }, { 170, 0.45 } };


/* tags an integer according to dylan format */
static mps_word_t dylan_int(mps_word_t x)
{
  return (x << 2)|1;
}


/* converts a dylan format int to an int (untags) */
static mps_word_t dylan_int_int(mps_word_t x)
{
  return x >> 2;
}


/* note: static, so auto-initialised to NULL */
static void *root[rootCOUNT];


/* churn -- allocate a lot of stuff (unreachable garbage, so it will */
/* probably only ever cause a minor collection). */
static void churn(mps_ap_t ap)
{
  int i;
  mps_addr_t p;
  mps_res_t e;

  for (i = 0; i < churnFACTOR; ++i) {
    do {
      MPS_RESERVE_BLOCK(e, p, ap, 4096);
      die(e, "MPS_RESERVE_BLOCK");
      die(dylan_init(p, 4096, root, 1), "dylan_init");
    } while (!mps_commit(ap, p, 4096));
  }
  p = NULL;
}


enum {
  rootSTATE,
  deadSTATE,
  finalizableSTATE,
  finalizedSTATE
};


static void test(mps_arena_t arena, mps_pool_class_t pool_class)
{
  size_t i;                     /* index */
  mps_ap_t ap;
  mps_fmt_t fmt;
  mps_chain_t chain;
  mps_pool_t pool;
  mps_res_t e;
  mps_root_t mps_root[2];
  mps_addr_t nullref = NULL;
  int state[rootCOUNT];
  mps_message_t message;
  size_t messages = 0;
  void *p;

  printf("---- finalcv: pool class %s ----\n", pool_class->name);

  die(mps_fmt_create_A(&fmt, arena, dylan_fmt_A()), "fmt_create\n");
  die(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_CHAIN, chain);
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, fmt);
    die(mps_pool_create_k(&pool, arena, pool_class, args), "pool_create\n");
  } MPS_ARGS_END(args);
  die(mps_root_create_table(&mps_root[0], arena, mps_rank_exact(), (mps_rm_t)0,
                            root, (size_t)rootCOUNT),
      "root_create\n");
  die(mps_root_create_table(&mps_root[1], arena, mps_rank_exact(), (mps_rm_t)0,
                            &p, (size_t)1),
      "root_create\n");
  die(mps_ap_create(&ap, pool, mps_rank_exact()), "ap_create\n");

  /* Make registered-for-finalization objects. */
  /* <design/poolmrg/#test.promise.ut.alloc> */
  for(i = 0; i < rootCOUNT; ++i) {
    do {
      MPS_RESERVE_BLOCK(e, p, ap, vectorSIZE);
      die(e, "MPS_RES_OK");
      die(dylan_init(p, vectorSIZE, &nullref, 1), "dylan_init");
    } while (!mps_commit(ap, p, vectorSIZE));

    /* store index in vector's slot */
    ((mps_word_t *)p)[vectorSLOT] = dylan_int(i);

    die(mps_finalize(arena, &p), "finalize\n");
    root[i] = p; state[i] = rootSTATE;
  }
  p = NULL;

  mps_message_type_enable(arena, mps_message_type_finalization());

  /* <design/poolmrg/#test.promise.ut.churn> */
  while (messages < messageCOUNT) {
    
    /* Perhaps cause (minor) collection */
    churn(ap);
    
    /* Maybe make some objects ready-to-finalize */
    /* <design/poolmrg/#test.promise.ut.drop> */
    for (i = 0; i < rootCOUNT; ++i) {
      if (root[i] != NULL && state[i] == rootSTATE) {
        if (rnd() % finalizationRATE == 0) {
          /* for this object, either... */
          if (rnd() % 2 == 0) {
            /* ...definalize it, or */
            die(mps_definalize(arena, &root[i]), "definalize\n");
            state[i] = deadSTATE;
          } else {
            /* ...expect it to be finalized soon */
            state[i] = finalizableSTATE;
          }
          /* Drop the root reference to it; this makes it */
          /* non-E-reachable: so either dead, or ready-to-finalize. */
          root[i] = NULL;
        }
      }
    }

    /* Test any finalized objects, and perhaps resurrect some */
    while (mps_message_poll(arena)) {
      mps_word_t *obj;
      mps_word_t objind;
      mps_addr_t objaddr;

      /* <design/poolmrg/#test.promise.ut.message> */
      cdie(mps_message_get(&message, arena, mps_message_type_finalization()),
           "get");
      cdie(0 == mps_message_clock(arena, message),
           "message clock should be 0 (unset) for finalization messages");
      mps_message_finalization_ref(&objaddr, arena, message);
      obj = objaddr;
      objind = dylan_int_int(obj[vectorSLOT]);
      printf("Finalizing: object %"PRIuLONGEST" at %p\n",
             (ulongest_t)objind, objaddr);
      /* <design/poolmrg/#test.promise.ut.final.check> */
      cdie(root[objind] == NULL, "finalized live");
      cdie(state[objind] == finalizableSTATE, "finalized dead");
      state[objind] = finalizedSTATE;
      /* sometimes resurrect */
      if (rnd() % 2 == 0)
        root[objind] = objaddr;
      mps_message_discard(arena, message);
      ++ messages;
    }
  }

  mps_ap_destroy(ap);
  mps_root_destroy(mps_root[1]);
  mps_root_destroy(mps_root[0]);
  mps_pool_destroy(pool);
  mps_chain_destroy(chain);
  mps_fmt_destroy(fmt);
}


int main(int argc, char *argv[])
{
  mps_arena_t arena;

  testlib_init(argc, argv);

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "arena_create\n");

  test(arena, mps_class_amc());
  test(arena, mps_class_amcz());
  test(arena, mps_class_awl());
  test(arena, mps_class_ams());
  test(arena, mps_class_lo());

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
