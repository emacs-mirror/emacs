/* extcon.c: ARENA EXTENDED AND CONTRACTED CALLBACK TEST
 *
 * $Id$
 * Copyright (c) 2022-2023 Ravenbrook Limited.  See end of file for license.
 *
 * .overview: This test case allocates a bunch of large objects, of a size 
 * similar to the size of the arena, to force the arena to extend. It then
 * discards the base pointers to those objects, and forces a collection.
 *
 * .limitations: This test checks that the EXTENDED and CONTRACTED
 * callbacks were called at least once, and that they are called the
 * same number of times. It does not check that the extensions and
 * contractions themselves were performed correctly, nor does it check
 * that an appropriate number of extensions and contractions took
 * place, nor does it check that they took place at sensible times.
 *
 * .dylan: This test uses Dylan format objects in common with most
 * other tests for convenience and brevity.
 */

#include "mps.h"
#include "testlib.h"
#include "fmtdy.h"
#include "fmtdytst.h"
#include "mpsavm.h"
#include "mpscamc.h"
#include <stdio.h>
#include <stdlib.h>

/* Number of test objects to allocate */
#define N_TESTOBJ 100
/* Number of dylan "slots" in each test object */
#define N_SLOT_TESTOBJ 10000
/* This is the number of bytes the initial arena is bigger than the test object size */
#define SIZEDIFF 10

/* Set alignment to mps_word_ts */
#define ALIGNMENT sizeof(mps_word_t)

/* Align size upwards to the next multiple of the word size. */
#define ALIGN_WORD(size) \
    (((size) + ALIGNMENT - 1) & ~(ALIGNMENT - 1))

/* Global objects*/
static mps_arena_t arena;       /* the arena */
static mps_pool_t obj_pool;     /* pool for test objects */
static mps_ap_t obj_ap;         /* allocation point used to allocate objects */

/* Count of number of arena contractions and extensions */
static int n_contract = 0;
static int n_extend = 0;

/* Callback functions for arena extension and contraction */
static void arena_extended_cb(mps_arena_t arena_in, mps_addr_t addr, size_t size)
{
  testlib_unused(arena_in);
  testlib_unused(addr);
  testlib_unused(size);
  printf("Arena extended by %"PRIuLONGEST" bytes\n", (ulongest_t)size);
  n_extend++;
}

static void arena_contracted_cb(mps_arena_t arena_in, mps_addr_t addr, size_t size)
{
  testlib_unused(arena_in);
  testlib_unused(addr);
  testlib_unused(size);
  printf("Arena contracted by %"PRIuLONGEST" bytes\n", (ulongest_t)size);
  n_contract++;
}

/* Messages for testbench debugging */
static void print_messages(void)
{
  mps_message_type_t type;

  while (mps_message_queue_type(&type, arena)) {
    mps_message_t message;

    cdie(mps_message_get(&message, arena, type),
         "get");

    switch(type) {
      case mps_message_type_gc_start():
        printf("GC start at %"PRIuLONGEST": %s\n",
               (ulongest_t)mps_message_clock(arena, message),
               mps_message_gc_start_why(arena, message));
        break;

      case mps_message_type_gc():
        printf("GC end at %"PRIuLONGEST" "
               "condemned %"PRIuLONGEST" "
               "not condemned %"PRIuLONGEST" "
               "live %"PRIuLONGEST"\n",
               (ulongest_t)mps_message_clock(arena, message),
               (ulongest_t)mps_message_gc_condemned_size(arena, message),
               (ulongest_t)mps_message_gc_not_condemned_size(arena, message),
               (ulongest_t)mps_message_gc_live_size(arena, message));
        break;

      default:
        cdie(0, "message type");
        break;
    }

    mps_message_discard(arena, message);
  }
}

/* Disabling inlining is necessary (but perhaps not sufficient) if using stack roots.
   See comment below with link to GitHub issue*/
ATTRIBUTE_NOINLINE
static void test_main(void *cold_stack_end)
{
  mps_fmt_t obj_fmt;
  mps_thr_t thread;
  mps_root_t stack_root, testobj_root;
  size_t arena_size, obj_size;
  int i;
  /* In the original version of extcon this was a stack root, but we
     observed unreliable failures to do with registering the cold end
     of the stack.  See GitHub issue #210
     <https://github.com/Ravenbrook/mps/issues/210>.  For now, we
     declare this as a separate root. */
  static mps_word_t testobj[N_TESTOBJ];

  /* The testobj array must be below (on all current Posix platforms)
     the cold end of the stack in order for the MPS to scan it.  We
     have observed a Heisenbug where GCC will inline test_main into
     main and lose this condition if the expression below is removed.
     This is a problem we are analysing in GitHub issue #210
     <https://github.com/Ravenbrook/mps/issues/210>.  For now, we
     disable this Insist to allow the test to run with a static
     testobj array. */
#if 0
  Insist((void *)&testobj[N_TESTOBJ] <= cold_stack_end);
  if ((void *)&testobj[N_TESTOBJ] > cold_stack_end)
    printf("Cold stack marker invalid!\n");
  else
    printf("Cold stack marker probably valid.\n");
#endif

  /* Make initial arena size slightly bigger than the test object size to force an extension as early as possible */
  /* See definition of make_dylan_vector() in fmtdytst.c for calculation of vector size */  
  obj_size = ALIGN_WORD((N_SLOT_TESTOBJ + 2) * sizeof(mps_word_t));
  arena_size = ALIGN_WORD(obj_size + SIZEDIFF);

  /* Create arena and register callbacks */
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, arena_size);
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_EXTENDED, (mps_fun_t)&arena_extended_cb);
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_CONTRACTED, (mps_fun_t)&arena_contracted_cb);
    die(mps_arena_create_k(&arena, mps_arena_class_vm(), args), "mps_arena_create_k");
  } MPS_ARGS_END(args);

  printf("Initial reservation %"PRIuLONGEST".\n", (ulongest_t)mps_arena_reserved(arena));

  die(dylan_fmt(&obj_fmt, arena), "dylan_fmt()");

  /* Create new pool */
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, obj_fmt);
    die(mps_pool_create_k(&obj_pool, arena, mps_class_amcz(), args),
      "mps_pool_create_k");
  } MPS_ARGS_END(args);

  /* Register thread */
  die(mps_thread_reg(&thread, arena), "Thread reg");

  /* Register stack roots */
  /* Since this testbench is currently not using a stack root, #IF 0 this out */
  testlib_unused(cold_stack_end);
  testlib_unused(stack_root);
#if 0
  die(mps_root_create_thread(&stack_root, arena, thread, cold_stack_end), "Create Stack root");
#endif

  /* Register ambiguous array of object roots. */
  die(mps_root_create_area(&testobj_root, arena,
                            mps_rank_ambig(), (mps_rm_t)0,
                            &testobj[0], &testobj[N_TESTOBJ],
                            mps_scan_area, NULL),
      "root_create_area(testobj)");

  /* Create allocation point */
  die(mps_ap_create_k(&obj_ap, obj_pool, mps_args_none), "Create Allocation point");

  mps_message_type_enable(arena, mps_message_type_gc_start());
  mps_message_type_enable(arena, mps_message_type_gc());

  /* Allocate objects and force arena extension */
  for (i = 0; i < N_TESTOBJ; i++) {

    die(make_dylan_vector(&testobj[i], obj_ap, N_SLOT_TESTOBJ), "make_dylan_vector"); 

    printf("Object %d committed.  "
           "Arena reserved: %"PRIuLONGEST".\n",
           i,
           (ulongest_t)mps_arena_reserved(arena));

    print_messages();
  }

  /* overwrite all the references to the objects*/
  for (i = 0; i < N_TESTOBJ; i++) {

    /* bonus test of mps_addr_object */
#if 0 /* Comment this out until mps_addr_object becomes available. */
    mps_addr_t out;
    Insist(N_TESTOBJ <= N_INT_TESTOBJ);

    /* use "i" to as a convenient way to generate different interior pointers
       To guarantee the i index will give us an interior pointer the number of test
       objects must be <= the number of integers in each object */
    Insist(N_TESTOBJ <= N_INT_TESTOBJ);
    die(mps_addr_object(&out, arena, &(testobj[i])->int_array[i]), "Address object");

    Insist(out == testobj[i]);

    /* end piggy back testbench */
#endif

    /* now overwrite the ref */
    testobj[i] = (mps_word_t)NULL;

    print_messages();
  }

  /* Collect */
  mps_arena_collect(arena);

  print_messages();

  /* Clean up */
  mps_root_destroy(testobj_root);
  /* mps_root_destroy(stack_root);*/ /*commented out while not using stack root */
  mps_thread_dereg(thread);
  mps_ap_destroy(obj_ap);
  mps_pool_destroy(obj_pool);
  mps_fmt_destroy(obj_fmt);
  mps_arena_destroy(arena);

  /* Destroying the arena should cause contraction callbacks on all
     remaining chunks, even if they had contents. */
  Insist(n_extend == n_contract);

  printf("Arena extended %d times\n", n_extend);
  printf("Arena contracted %d times\n", n_contract);

  /* comment out some diagnostics for investigating issue #210 mentioned above */
#if 0
  printf("&testobj[N_TESTOBJ] = %p\n", (void *)&testobj[N_TESTOBJ]);
  printf("cold_stack_end = %p\n", cold_stack_end);
#endif
  if (n_extend == 0) 
    printf("No callbacks received upon arena extended!\n");
  if (n_contract == 0)
    printf("No callbacks received upon arena contracted!\n");

  if (n_contract == 0 || n_extend == 0)
    exit(EXIT_FAILURE);
}

int main(int argc, char* argv[])
{
  void *stack_marker = &stack_marker;

  testlib_init(argc, argv);

  test_main(stack_marker);

  printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);

  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2022-2023 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
