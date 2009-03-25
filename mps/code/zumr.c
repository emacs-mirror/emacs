/* zumr.c: UnManaged References test
 *
 * $Id$
 * Copyright (c) 2008 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * OBJECTIVE
 *
 * Test MPS collections.  In particular, reporting of how collections 
 * progress.
 *
 * Please add tests for other collection behaviour into this file.  
 * (It's easier to maintain a few big tests than myriad small tests).
 * Expand the script language as necessary!  RHSK 2008-12-22.
 *
 *
 * DESIGN OVERVIEW
 *
 * Each script runs in a newly created arena.
 *
 * [preliminary, incomplete, code still being written]
 * The commands are:
 *   Arena -- governs initial arena size, required, must be first
 *   Make -- makes some objects, stores a proportion (chosen at 
 *           random) in the specified myroot array slots, and 
 *           drops the rest (which therefore become garbage)
 *   Katalog -- (will be renamed Catalog) makes a Catalog, which 
 *           is a 40 MB 4-level tree of 10^5 objects; see .catalog;
 *           see also .catalog.broken.
 *   Collect -- request a synchronous full garbage collection
 *
 *
 * CODE OVERVIEW
 *
 * main() has the list of testscripts.  
 *
 * testscriptA() sets up a new arena and trampolines to testscriptB().
 *
 * testscriptB() creates pools and objects for this test script.
 *
 * testscriptC() actually runs the script.
 *
 *
 * DEPENDENCIES
 *
 * This test uses the dylan object format, but the reliance on this
 * particular format is not great and could be removed.
 *
 *
 * BUGS, FUTURE IMPROVEMENTS, ETC
 *
 * HISTORY
 *
 * This code was created by first copying <code/zmess.c>.
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
#include <time.h>  /* clock */
#include <string.h>  /* strlen */


#ifdef MPS_BUILD_MV
/* MSVC warning 4996 = stdio / C runtime 'unsafe' */
/* Objects to: sscanf.  See job001934. */
#pragma warning( disable : 4996 )
#endif


/* testChain -- generation parameters for the test */
#define genCOUNT 2
static mps_gen_param_s testChain[genCOUNT] = {
  { 100, 0.85 }, { 170, 0.45 } };


/* myroot -- array of exact references that are the root
 *
 * (note: static, so pointers are auto-initialised to NULL)
 */
#define myrootCOUNT 30000
static void *myroot[myrootCOUNT];
static void *myunmanaged[10];

/* testscriptC -- do stuff
 */
static void testscriptC(mps_arena_t arena, mps_ap_t ap, mps_root_t root_stackreg)
{
  mps_word_t v;
  enum {thingSig = 0x00ccccc0 >> 2};

  printf("  U1()\n");
  die(make_dylan_vector(&v, ap, 1), "make_dylan_vector");
  DYLAN_VECTOR_SLOT(v, 0) = DYLAN_INT(thingSig);
  myroot[0] = (void*)v;
  printf("myroot[0] = %p\n", myroot[0]);
  Insist(DYLAN_VECTOR_SLOT(myroot[0], 0) = DYLAN_INT(thingSig));
  v = 0;
  
  /* Ru */
  myunmanaged[0] = myroot[0];
  printf("myunmanaged[0] = %p\n", myunmanaged[0]);
  Insist(DYLAN_VECTOR_SLOT(myunmanaged[0], 0) = DYLAN_INT(thingSig));
  
  /* Flip */
  printf("fill this page with junk, so should be only junk, so unmapped by GC.\n");
  die(make_dylan_vector(&v, ap, 10000), "make_dylan_vector");
  v = 0;

  printf("flip.\n");
  mps_root_destroy(root_stackreg);
  mps_arena_collect(arena);
  
  /* Ra */
  printf("myroot[0] = %p\n", myroot[0]);
  printf("myunmanaged[0] = %p\n", myunmanaged[0]);
  Insist(DYLAN_VECTOR_SLOT(myroot[0], 0) = DYLAN_INT(thingSig));
  
  /* Check */
  if(myunmanaged[0] != myroot[0]) {
    printf("Move detected.\n");
    exit(1);
  }
}


/* testscriptB -- create pools and objects
 *
 * Is called via mps_tramp, so matches mps_tramp_t function prototype,
 * and use trampDataStruct to pass parameters.
 */

typedef struct trampDataStruct {
  mps_arena_t arena;
  mps_thr_t thr;
  const char *script;
} trampDataStruct;

static void *testscriptB(void *arg, size_t s)
{
  trampDataStruct trampData;
  mps_arena_t arena;
  mps_thr_t thr;
  const char *script;
  mps_fmt_t fmt;
  mps_chain_t chain;
  mps_pool_t amc;
  int i;
  mps_root_t root_table;
  mps_ap_t ap;
  mps_root_t root_stackreg;
  void *stack_starts_here;  /* stack scanning starts here */

  Insist(s == sizeof(trampDataStruct));
  trampData = *(trampDataStruct*)arg;
  arena = trampData.arena;
  thr = trampData.thr;
  script = trampData.script;

  die(mps_fmt_create_A(&fmt, arena, dylan_fmt_A()), "fmt_create");
  die(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");
  die(mps_pool_create(&amc, arena, mps_class_amc(), fmt, chain),
      "pool_create amc");
  for(i = 0; i < myrootCOUNT; ++i) {
    myroot[i] = NULL;
  }
  die(mps_root_create_table(&root_table, arena, MPS_RANK_EXACT, (mps_rm_t)0,
                            myroot, (size_t)myrootCOUNT),
      "root_create");
  die(mps_ap_create(&ap, amc, MPS_RANK_EXACT), "ap_create");
  
  /* root_stackreg: stack & registers are ambiguous roots = mutator's workspace */
  die(mps_root_create_reg(&root_stackreg, arena,
                          mps_rank_ambig(), (mps_rm_t)0, thr,
                          mps_stack_scan_ambig, &stack_starts_here, 0),
      "root_stackreg");


  mps_message_type_enable(arena, mps_message_type_gc_start());
  mps_message_type_enable(arena, mps_message_type_gc());
  mps_message_type_enable(arena, mps_message_type_finalization());

  testscriptC(arena, ap, root_stackreg);

  /* mps_root_destroy(root_stackreg); -- destroyed in testscriptC */
  mps_ap_destroy(ap);
  mps_root_destroy(root_table);
  mps_pool_destroy(amc);
  mps_chain_destroy(chain);
  mps_fmt_destroy(fmt);

  return NULL;
}


/* testscriptA -- create arena, thr, and tramp; call testscriptB
 */
static void testscriptA(const char *script)
{
  mps_arena_t arena;
  int si, sb;  /* sscanf items, sscanf bytes */
  unsigned long arenasize = 0;
  mps_thr_t thr;
  mps_tramp_t trampFunction;
  trampDataStruct trampData;
  void *trampResult;

  si = sscanf(script, "Arena(size %lu)%n", &arenasize, &sb);
  cdie(si == 1, "bad script command: Arena(size %%lu)");
  script += sb;
  printf("  Create arena, size = %lu.\n", arenasize);

  /* arena */
  die(mps_arena_create(&arena, mps_arena_class_vm(), arenasize),
      "arena_create");

  /* thr: used to stop/restart multiple threads */
  die(mps_thread_reg(&thr, arena), "thread");

  /* tramp: used for protection (barrier hits) */
  /* call testscriptB! */
  trampFunction = testscriptB;
  trampData.arena = arena;
  trampData.thr = thr;
  trampData.script = script;
  mps_tramp(&trampResult, trampFunction, &trampData, sizeof trampData);

  mps_thread_dereg(thr);
  mps_arena_destroy(arena);

  printf("  Destroy arena etc.\n\n");

}


/* main -- runs various test scripts
 *
 */
int main(int argc, char **argv)
{

  randomize(argc, argv);

  /* test unmanaged */
  testscriptA("Arena(size 524288), U1().");

  fflush(stdout); /* synchronize */
  fprintf(stderr, "\nConclusion:  Failed to find any defects.\n");
  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002, 2008 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
