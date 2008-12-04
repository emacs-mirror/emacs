/* fin1658a.c: Test for suspected defect described in job001658
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
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
 * This code was created by first copying <code/finalcv.c>.
 */

#include "testlib.h"
#include "mps.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "fmtdy.h"
#include "fmtdytst.h"
#include "mpstd.h"
#ifdef MPS_OS_W3
#include "mpsw3.h"
#endif
#include <stdlib.h>


#define testArenaSIZE   ((size_t)16<<20)

/* usually (ArenaAlign / sizeof(Ref)) = 1024 */
/* so choose 3000 to force 3 segments of guardians */
#define myrootCOUNT 3000

/* testChain -- generation parameters for the test */
#define genCOUNT 2
static mps_gen_param_s testChain[genCOUNT] = {
  { 150, 0.85 }, { 170, 0.45 } };


/* note: static, so auto-initialised to NULL */
static void *myroot[myrootCOUNT];
static int state[myrootCOUNT];


enum {
  rootSTATE,
  deadSTATE,
  finalizableSTATE,
  finalizedSTATE
};


/* report -- collect messages, report, and check objects were */
/* legally finalized. */
static void report(mps_arena_t arena, int expect)
{
  int found = 0;

  /* Test any finalized objects */
  while (mps_message_poll(arena)) {
    mps_message_t message;
    mps_word_t *obj;
    mps_word_t objind;
    mps_addr_t objaddr;

    cdie(mps_message_get(&message, arena, mps_message_type_finalization()),
         "get");

    found += 1;
    mps_message_finalization_ref(&objaddr, arena, message);
    obj = objaddr;
    objind = DYLAN_INT_INT(DYLAN_VECTOR_SLOT(obj, 0));
    printf("Finalizing: object %lu at %p\n", objind, objaddr);
    cdie(myroot[objind] == NULL, "finalized live");
    cdie(state[objind] == finalizableSTATE, "not finalizable");
    state[objind] = finalizedSTATE;
    mps_message_discard(arena, message);
  }
  
  if(found < expect) {
    printf("...expected %d finalizations, but got fewer: only %d!\n", 
           expect, found);
    cdie(FALSE, "wrong number of finalizations");
  } else if(found > expect) {
    printf("...expected %d finalizations, but got more: %d!\n", 
           expect, found);
    cdie(FALSE, "wrong number of finalizations");
  }
}


typedef struct trampDataStruct {
  mps_arena_t arena;
  mps_thr_t thr;
} trampDataStruct;


static void *test(void *arg, size_t s)
{
  trampDataStruct trampData;
  mps_arena_t arena;
  mps_thr_t thr;
  mps_fmt_t fmt;
  mps_chain_t chain;
  mps_pool_t amc;
  mps_root_t root_table;
  mps_ap_t ap;
  mps_root_t root_stackreg;
  int i;
  int N = myrootCOUNT - 1;
  void *stack_starts_here;  /* stack scanning starts here */

  trampData = *(trampDataStruct*)arg;
  arena = trampData.arena;
  thr = trampData.thr;
  (void)s;

  die(mps_fmt_create_A(&fmt, arena, dylan_fmt_A()), "fmt_create");
  die(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");
  die(mps_pool_create(&amc, arena, mps_class_amc(), fmt, chain),
      "pool_create amc");
  die(mps_root_create_table(&root_table, arena, MPS_RANK_EXACT, (mps_rm_t)0,
                            myroot, (size_t)myrootCOUNT),
      "root_create");
  die(mps_ap_create(&ap, amc, MPS_RANK_EXACT), "ap_create");
  
  /* root_stackreg: stack & registers are ambiguous roots = mutator's workspace */
  die(mps_root_create_reg(&root_stackreg, arena,
                          mps_rank_ambig(), (mps_rm_t)0, thr,
                          mps_stack_scan_ambig, &stack_starts_here, 0),
      "root_stackreg");

  /* Make myrootCOUNT registered-for-finalization objects. */
  /* <design/poolmrg/#test.promise.ut.alloc> */
  for(i = 0; i < myrootCOUNT; ++i) {
    mps_word_t v;
    die(make_dylan_vector(&v, ap, 2), "make_dylan_vector");
    DYLAN_VECTOR_SLOT(v, 0) = DYLAN_INT(i);
    DYLAN_VECTOR_SLOT(v, 1) = (mps_word_t)NULL;
    die(mps_finalize(arena, (mps_addr_t*)&v), "finalize");
    myroot[i] = (void*)v;
    state[i] = rootSTATE;
  }

  /* stop stack scanning, to prevent unwanted object retention */
  mps_root_destroy(root_stackreg);

  mps_message_type_enable(arena, mps_message_type_finalization());

  mps_arena_collect(arena);
  report(arena, 0);

  /* make 0 and N finalizable */
  myroot[0] = NULL;
  state[0] = finalizableSTATE;
  myroot[N] = NULL;
  state[N] = finalizableSTATE;
  mps_arena_collect(arena);
  report(arena, 2);

  mps_arena_collect(arena);
  report(arena, 0);

  /* make 1 and N-1 refer to each other and finalizable */
  DYLAN_VECTOR_SLOT(myroot[1]  , 1) = (mps_word_t)myroot[N-1];
  DYLAN_VECTOR_SLOT(myroot[N-1], 1) = (mps_word_t)myroot[1];
  myroot[1] = NULL;
  state[1] = finalizableSTATE;
  myroot[N-1] = NULL;
  state[N-1] = finalizableSTATE;
  mps_arena_collect(arena);
  report(arena, 2);

  mps_arena_collect(arena);
  report(arena, 0);

  /* @@@@ <design/poolmrg/#test.promise.ut.nofinal.check> missing */

  mps_ap_destroy(ap);
  mps_root_destroy(root_table);
  mps_pool_destroy(amc);
  mps_chain_destroy(chain);
  mps_fmt_destroy(fmt);

  return NULL;
}


int main(int argc, char **argv)
{
  mps_arena_t arena;
  mps_thr_t thr;
  mps_tramp_t trampFunction;
  trampDataStruct trampData;
  void *trampResult;

  randomize(argc, argv);

  /* arena */
  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "arena_create");
  
  /* thr: used to stop/restart multiple threads */
  die(mps_thread_reg(&thr, arena), "thread");
  
  /* tramp: used for protection (barrier hits) */
  trampFunction = test;
  trampData.arena = arena;
  trampData.thr = thr;
  mps_tramp(&trampResult, trampFunction, &trampData, sizeof trampData);

  mps_thread_dereg(thr);
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
