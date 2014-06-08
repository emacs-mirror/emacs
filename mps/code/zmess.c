/* zmess.c: Message test
 *
 * $Id$
 * Copyright (c) 2008-2014 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * OBJECTIVE
 *
 * Test MPS messages.  In particular:
 *  - Check prompt finalization even when there are several segs 
 *    of guardians.  This test replaces fin1658a.c.  See job001658.
 *  - Check GC messages are correctly generated, posted, and queued,
 *    regardless of when the client gets them.  (Note: "get" means 
 *    "mps_message_get", throughout).  See job001989.
 *
 * Please add tests for other message behaviour into this file.  
 * Expand the script language as necessary!  RHSK 2008-12-19.
 *
 *
 * DESIGN OVERVIEW
 *
 * Client (this test) does various actions that are known to provoke 
 * MPS messages.  Client (this test) gets these messages at variable 
 * times.
 *
 * Verification is:
 *   - client gets all the expected messages, and no others, at the 
 *     expected time;
 *   - no asserts or failures.
 *
 * Additionally: client checks the message order.  MPS specification 
 * does not currently guarantee that messages are queued in order of 
 * posting, but in fact they should be, and it is a useful check.
 * (But finalization messages from a single collection may be posted 
 * in any order).
 *
 * The actions, messages to check for, and get times, are scripted 
 * using a simple text code:
 *   C - action: request garbage-collection;
 *   F - action: make a (registered)finalized object unreachable
 *        (note: this drops the myroot ref, but some objects are 
 *         deliberately kept alive by additional references; see 
 *         .keep-alive)
 *   b - message produced: collection begin (mps_message_type_gc_start);
 *   e - message produced: collection end (mps_message_type_gc);
 *   f - message produced: finalization (mps_message_type_finalization);
 *   . - get messages.
 *   ! - get messages without discarding (see .discard).
 *
 * Example:
 *  script "Cbe.FFCbffe.Cbe"
 *  means:
 *    Request a collection and check for _gc_start and _gc messages
 *    (in that order, and no other messages).  Then drop refs to two 
 *    objects, request another collection, and check for _gc_start, 
 *    the two finalization messages (in either order), and _gc.  Then 
 *    request a third collection and end the test WITHOUT GETTING 
 *    the last two messages (note: no "."), to test that 
 *    mps_arena_destroy copes with ungot messages.
 *
 * Each script runs in a newly created arena. The arena is clamped so
 * that collections only happen when the script requests them.
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
 * There are a few special objects with refs to each other (see 
 * .keep-alive).  For clarity and flexibility, there should be special 
 * actions to drop the myroot ref to these, eg. '0', '1', '2', 'Y', 'Z'.
 * Whereas (for clarity) 'F' should be an action that drops the myroot 
 * ref to a plain (non-kept-alive) object, thereby simply provoking a 
 * single finalization message.
 *
 * Actions could be expanded to include:
 *   - mps_arena_start_collect;
 *   - mps_arena_step;
 *   - automatic (not client-requested) collections.
 * etc.
 *
 * HISTORY
 *
 * This code was created by first copying <code/fin1658a.c>.
 */

#include "testlib.h"
#include "mpslib.h"
#include "mps.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "fmtdy.h"
#include "fmtdytst.h"
#include "mpstd.h"

#include <stdio.h> /* printf */


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
enum {
  rootSTATE,
  deadSTATE,
  finalizableSTATE,
  finalizedSTATE
};
static int state[myrootCOUNT];


/* report -- get and check messages
 *
 * Get messages, report what was got, check they are the expected 
 * messages, and (for finalization messages) check that these objects 
 * should have been finalized (because we made them unreachable).
 *
 * .discard: The client should always call mps_message_discard when 
 * it has finished with the message.  But calling with the "discard" 
 * parameter set to false lets us check how the MPS handles naughty 
 * clients.  The undiscarded messages must be cleared up by 
 * ArenaDestroy.
 */
static void report(mps_arena_t arena, const char *pm, Bool discard)
{
  int found = 0;
  char mFound = '\0';
  mps_message_type_t type;

  while (mps_message_queue_type(&type, arena)) {
    mps_message_t message;
    mps_word_t *obj;
    mps_word_t objind;
    mps_addr_t objaddr;

    cdie(mps_message_get(&message, arena, type),
         "get");
    found += 1;
    
    switch(type) {
      case mps_message_type_gc_start(): {
        printf("    Begin Collection\n");
        mFound = 'b';
        break;
      }
      case mps_message_type_gc(): {
        printf("    End Collection\n");
        mFound = 'e';
        break;
      }
      case mps_message_type_finalization(): {
        mps_message_finalization_ref(&objaddr, arena, message);
        obj = objaddr;
        objind = DYLAN_INT_INT(DYLAN_VECTOR_SLOT(obj, 0));
        printf("    Finalization for object %"PRIuLONGEST" at %p\n",
               (ulongest_t)objind, objaddr);
        cdie(myroot[objind] == NULL, "finalized live");
        cdie(state[objind] == finalizableSTATE, "not finalizable");
        state[objind] = finalizedSTATE;
        mFound = 'f';
        break;
      }
      default: {
        cdie(0, "message type");
        break;
      }
    }
    
    if(discard) {
      mps_message_discard(arena, message);  /* .discard */
    }

    cdie('\0' != *pm, "Found message, but did not expect any");
    cdie(mFound == *pm, "Found message type != Expected message type");
    pm++;
  }
  
  mFound = '\0';
  cdie(mFound == *pm, "No message found, but expected one");
}


/* testscriptC -- actually runs a test script
 *
 */
static void testscriptC(mps_arena_t arena, const char *script)
{
  unsigned isLoNext = 1;
  unsigned loNext = 0;
  unsigned hiNext = myrootCOUNT - 1;
  unsigned i;
  const char *scriptAll = script;
  char am[100];  /* Array of Messages (expected but not yet got) */
  char *pmNext = am;  /* Pointer to where Next Message will be stored */

  while(*script != '\0') {
    switch(*script) {
      case '.': {
        *pmNext = '\0';
        printf("  Getting messages (expecting \"%s\")...\n", am);
        report(arena, am, TRUE);
        printf("  ...done.\n");
        pmNext = am;
        break;
      }
      case '!': {
        /* Like '.', but not discarding got messages; see .discard */
        *pmNext = '\0';
        printf("  Getting messages (expecting \"%s\")...\n", am);
        report(arena, am, FALSE);  /* FALSE: see .discard */
        printf("  ...done.\n");
        printf("  NOTE: DELIBERATELY FAILING TO DISCARD MESSAGES, "
               "TO SEE HOW MPS COPES.\n");  /* .discard */
        pmNext = am;
        break;
      }
      case 'C': {
        printf("  Collect\n");
        die(mps_arena_collect(arena), "mps_arena_collect");
        break;
      }
      case 'F': {
        /* (perhaps) make an object Finalizable
         *
         * .alternate: We alternately pick objects from the low and 
         * high ends of the myroot array.  This is used to test for 
         * the defect described in job001658.
         */
        Insist(loNext <= hiNext);
        i = isLoNext ? loNext++ : hiNext--;
        isLoNext = 1 - isLoNext;
        
        printf("  Drop myroot ref to object %u -- "
               "this might make it Finalizable\n", i);
        /* drop myroot ref, to perhaps make i finalizable */
        /* (but see .keep-alive) */
        myroot[i] = NULL;
        state[i] = finalizableSTATE;
        break;
      }
      case 'b':
      case 'e':
      case 'f': {
        /* expect that MPS has posted a particular message */
        *pmNext++ = *script;
        break;
      }
      default: {
        printf("unknown script command %c (script %s).\n",
               *script, scriptAll);
        cdie(FALSE, "unknown script command");
        return;
      }
    }
    Insist(am <= pmNext && pmNext < am + NELEMS(am));
    script++;
  }
}


/* testscriptB -- create pools and objects; call testscriptC
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
  mps_root_t root_table;
  mps_ap_t ap;
  mps_root_t root_stackreg;
  size_t i;
  int N = myrootCOUNT - 1;
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
  die(mps_root_create_table(&root_table, arena, mps_rank_exact(), (mps_rm_t)0,
                            myroot, (size_t)myrootCOUNT),
      "root_create");
  die(mps_ap_create(&ap, amc, mps_rank_exact()), "ap_create");
  
  /* root_stackreg: stack & registers are ambiguous roots = mutator's workspace */
  die(mps_root_create_reg(&root_stackreg, arena,
                          mps_rank_ambig(), (mps_rm_t)0, thr,
                          mps_stack_scan_ambig, &stack_starts_here, 0),
      "root_stackreg");

  /* Make myrootCOUNT registered-for-finalization objects. */
  /* Each is a dylan vector with 2 slots, inited to: (index, NULL) */
  for(i = 0; i < myrootCOUNT; ++i) {
    mps_word_t v;
    mps_addr_t v_ref;
    die(make_dylan_vector(&v, ap, 2), "make_dylan_vector");
    DYLAN_VECTOR_SLOT(v, 0) = DYLAN_INT(i);
    DYLAN_VECTOR_SLOT(v, 1) = (mps_word_t)NULL;
    v_ref = (mps_addr_t)v;
    die(mps_finalize(arena, &v_ref), "finalize");
    myroot[i] = (void*)v;
    state[i] = rootSTATE;
  }
  
  /* .keep-alive: Create some additional inter-object references.
   *
   * 1 and N-1 don't die until myroot refs to both have been nulled.
   *
   * 2 and 3 don't die until myroot refs to both have been nulled.
   *
   * We do this to check that reachability via non-root refs prevents 
   * finalization.
   */

  /* Leave 0 and N containing NULL refs */
  
  /* Make 1 and N-1 refer to each other */
  DYLAN_VECTOR_SLOT(myroot[1]  , 1) = (mps_word_t)myroot[N-1];
  DYLAN_VECTOR_SLOT(myroot[N-1], 1) = (mps_word_t)myroot[1];

  /* Make 2 and 3 refer to each other */
  DYLAN_VECTOR_SLOT(myroot[2], 1) = (mps_word_t)myroot[3];
  DYLAN_VECTOR_SLOT(myroot[3], 1) = (mps_word_t)myroot[2];

  /* Stop stack scanning, otherwise stack or register dross from */
  /* these setup functions can cause unwanted object retention, */
  /* which would mean we don't get the finalization messages we */
  /* expect. */
  mps_root_destroy(root_stackreg);

  mps_message_type_enable(arena, mps_message_type_gc_start());
  mps_message_type_enable(arena, mps_message_type_gc());
  mps_message_type_enable(arena, mps_message_type_finalization());

  testscriptC(arena, script);

  mps_arena_park(arena);
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
  mps_thr_t thr;
  mps_tramp_t trampFunction;
  trampDataStruct trampData;
  void *trampResult;

  printf("Script: \"%s\"\n  Create arena etc.\n", script);

  /* arena */
  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "arena_create");
  mps_arena_clamp(arena);

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

/* TIMCA_remote -- TraceIdMessagesCreate Alloc remote control
 *
 * In low memory situations, ControlAlloc may be unable to allocate 
 * memory for GC messages.  This needs to work flawlessly, but is 
 * hard to test.
 *
 * To simulate it for testing purposes, add the following lines to 
 * traceanc.c, before the definition of TraceIdMessagesCreate:
 *    #define ControlAlloc !TIMCA_remote() ? ResFAIL : ControlAlloc
 *    extern Bool TIMCA_remote(void);
 * (See changelist 166959).
 *
 * TIMCA_remote returns a Bool, true for let "ControlAlloc succeed".
 */

#ifdef TEST_CONTROLALLOC_FAILURE

static const char *TIMCA_str = "";
static int TIMCA_done = 0;
static void TIMCA_setup(const char *string)
{
  /* TIMCA_setup -- TraceIdMessagesCreate Alloc remote control
   *
   * 1..9 -- succeed this many times
   * 0    -- fail once
   * NUL  -- succeed from now on
   *
   * Eg: "1400" succeeds 5 times, fails 2 times, then succeeds forever.
   */
  TIMCA_str = string;
  TIMCA_done = 0;
}

extern Bool TIMCA_remote(void);
Bool TIMCA_remote(void)
{
  Bool succeed;
  
  if(*TIMCA_str == '\0') {
    succeed = TRUE;
  } else if(*TIMCA_str == '0') {
    succeed = FALSE;
    TIMCA_str++;
  } else {
    Insist(*TIMCA_str >= '1' && *TIMCA_str <= '9');
    succeed = TRUE;
    TIMCA_done++;
    if(TIMCA_done == *TIMCA_str - '0') {
      TIMCA_done = 0;
      TIMCA_str++;
    }
  }
  
  return succeed;
}

#endif  /* TEST_CONTROLALLOC_FAILURE */


/* main -- runs various test scripts
 *
 */
int main(int argc, char *argv[])
{
  testlib_init(argc, argv);

  /* Scripts that should fail (uncomment to show failure is detected) */
  /*testscriptA("C.");*/
  /*testscriptA("b.");*/

  /* The most basic scripts */
  testscriptA(".");
  testscriptA("Cbe.");
  testscriptA("Cbe.Cbe.");

  /* Get messages, but not promptly */
  testscriptA(".Cbe.CbeCbeCbe.");

  /* Ungot messages at ArenaDestroy */
  testscriptA("Cbe");
  testscriptA("Cbe.CbeCbeCbe");

  /* Fail to call mps_message_discard */
  testscriptA("Cbe!");
  testscriptA("Cbe!CbeCbeCbe!");

  /* Simple finalization
   *
   * These tests rely on the particular order in which the "F" command 
   * nulls-out references.  Not every "F" makes an object finalizable.
   * See .keep-alive.
   */
  testscriptA("FFCbffe.");
  testscriptA("FFCbffe.FFCbffe.");
  testscriptA("FFCbffe.FCbe.F.Cbffe.FFCbfe.FF.Cbfffe.");
  
  /* Various other scripts */
  testscriptA("Cbe.FFCbffe.Cbe");

  /* Simulate low memory situations
   *
   * These scripts only work with a manually edited traceanc.c --
   * see TIMCA_remote() above.
   *
   * When TraceIdMessagesCreate is trying to pre-allocate GC messages, 
   * either "0" or "10" makes it fail -- "0" fails the trace start 
   * message alloc, whereas "10" fails the trace end message alloc. 
   * In either case TraceIdMessagesCreate promptly gives up, and 
   * neither start nor end message will be sent for the next trace.
   *
   * See <design/message-gc#lifecycle>.
   */
#if TEST_CONTROLALLOC_FAILURE
  {
    /* ArenaCreate unable to pre-allocate: THESE SHOULD FAIL */
    /* manually edit if(0) -> if(1) to test these */
    if(0) {
      TIMCA_setup("0"); testscriptA("Fail at create 1");
    }
    if(0) {
      TIMCA_setup("10"); testscriptA("Fail at create 2");
    }

    /* ArenaDestroy with no pre-allocated messages */
    TIMCA_setup("20"); testscriptA("Cbe.");
    TIMCA_setup("210"); testscriptA("Cbe.");

    /* Collect with no pre-allocated messages: drops messages, */
    /* hence "C." instead of "Cbe.".  Also, in diagnostic varieties, */
    /* these should produce a "droppedMessages" diagnostic at */
    /* ArenaDestroy. */
    TIMCA_setup("2022"); testscriptA("Cbe.C.Cbe.");
    TIMCA_setup("21022"); testscriptA("Cbe.C.Cbe.");

    /* 2 Collects and ArenaDestroy with no pre-allocated messages */
    TIMCA_setup("2000"); testscriptA("Cbe.C.C.");
    TIMCA_setup("201010"); testscriptA("Cbe.C.C.");
    
    TIMCA_setup("");  /* must reset it! */
  }
#endif

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
