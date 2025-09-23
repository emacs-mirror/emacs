/* addrobj.c: BASE ADDRESS FROM INTERIOR POINTER TEST
 *
 * Copyright (c) 2023 Ravenbrook Limited. See end of file for license.
 *
 * .overview This test is for mps_addr_object(). Its intention is to
 * verify that the function returns the appropriate base pointer to an
 * object when provided with an interior pointer. It also tests that the
 * function fails appropriately when the provided with a pointer to
 * unmanaged memory, or to an object in a pool that doesn't support this
 * feature.
 *
 * .limitations Objects that have been moved should cause the function to
 * fail with MPS_RES_FAIL, however this is not tested. It could be tested if
 * a testbench deliberately created a forwarding object, however this might
 * confuse a pool that does automatic garbage collection such as AMC or AMCZ,
 * so any such test would need to be designed to handle that.
 * This test only examines behaviour in AMCZ and MVFF pools, i.e. A pool (AMCZ)
 * which currently implements mps_addr_object() and one (MVFF) that doesn't.
 */

#include "mps.h"
#include "testlib.h"
#include "fmtdy.h"
#include "fmtdytst.h"
#include "mpsavm.h"
#include "mpscamc.h"
#include "mpscmvff.h"
#include "stdio.h"
#include <stdlib.h>

/* Define an object size to allocate. The size chosen doesn't matter much, except that this testbench assumes
   that the object is large enough that a pointer could point to the interior of the object, without also
   pointing to the base pointer of the object at the same time. For char pointers, this is probably 2 bytes.
   Since we are using the Dylan library, we define the size of the object in terms of Dylan slots. See
   fmtdytst.c for details of the Dylan object structure.*/
#define N_SLOT_TESTOBJ 100

static void test_main(void)
{
  mps_arena_t arena;
  mps_pool_t amcz_pool, mvff_pool;
  mps_ap_t obj_ap;
  mps_fmt_t obj_fmt;
  mps_root_t testobj_root;
  mps_res_t res;
  /* In another testbench (extcon.c) we observed unreliable failures to do with registering the cold end
     of the stack.  See GitHub issue #210
     <https://github.com/Ravenbrook/mps/issues/210>.  For now, we
     declare this as a separate root. */
  static mps_addr_t testobj;
  mps_addr_t out, in;

  /* Create arena */
  die(mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none), "mps_arena_create_k");


  /* INTRO TO TESTS: There are several tests. They test the expected "normal" operation of the
     function, using an interior pointer, also corner cases where the interior pointer equals the
     base pointer, where it equals the limit pointer. We also test asking about an address in unmanaged
     memory, and about an address in a pool which currently does not support mps_addr_object. If you write
     more tests, describe them here.*/


  /* TEST 1: Test using an interior pointer in an object in an AMCZ pool.
     At the time of writing this test, the AMCZ pool is the only pool where
     there exists a requirement to provide base addresses from interior pointers.
     Currently, the AMCZ pool (and by extension, the AMC pool which shares the same
     module as AMCZ) is the only pool for which mps_addr_object is implemented */

  /* Use the dylan format for convenience */
  die(dylan_fmt(&obj_fmt, arena), "dylan_fmt");

  /* Create the pool */
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, obj_fmt);
    die(mps_pool_create_k(&amcz_pool, arena, mps_class_amcz(), args), "mps_pool_create_k amcz");
  } MPS_ARGS_END(args);

  /* Create an area of ambiguous pointers to keep the object alive and in place, in this case
     the area only contains room for a single reference since we are only using one object to test */
  die(mps_root_create_area(&testobj_root, arena,
                           mps_rank_ambig(), (mps_rm_t)0,
                           &testobj, &testobj+1,
                           mps_scan_area, NULL),
          "mps_root_create_area");

  /* Create the allocation point */
  die(mps_ap_create_k(&obj_ap, amcz_pool, mps_args_none), "mps_ap_create_k");

  /* Make a Dylan object, size = (N_SLOT_TESTOBJ+2) * sizeof(mps_word_t).
     (See fmtdytst.c for size calculation) */
  {
    /* Because make_dylan_vector returns its pointer-to-object as an mps_word_t rather than an
       mps_addr_t, and commits the object, we need to somehow safely allocate our object without
       type punning and without risking that our object be destroyed.
       Rather than redefine our reference table with type mps_word_t, which hides the intention of the table,
       park the arena to disable garbage collection. Allocate our dylan object on the (unregistered) stack
       storing its address in an mps_word_t. Then store this mps_word_t as an mps_addr_t in our reference
       table, and release the arena since our object is now safely pinned.
       Another approach would be to create another static registered root for ambiguous references of type
       mps_word_t and then copy to the mps_addr_t root, which would avoid needing to park the arena.
    */
    mps_word_t p_word;
    mps_arena_park(arena);
    die(make_dylan_vector(&p_word, obj_ap, N_SLOT_TESTOBJ), "make_dylan_vector");
    /* If we hadn't parked the arena, our vector might have been GC'd here */
    testobj = (mps_addr_t)p_word;
    mps_arena_release(arena);
  }

  /* Construct a pointer to roughly halfway inside the object */
  in = (mps_addr_t)((char *)testobj + (N_SLOT_TESTOBJ/2) * sizeof(mps_word_t));

  /* Ensure that this is an interior pointer, and not the base pointer,
     since we want to make sure we are testing with a true interior pointer and not
     one that also happens to be the base pointer. This Insist is intended to protect
     against the testbench losing its ability to test "true" interior pointers (i.e. ones
     which don't match the base pointer) if the test object sizes were changed to be very
     small. Note that we don't currently consider the "limit" of the object as a corner case
     (so we don't Insist(in != limit) ) but we do consider limit+1, i.e. the pointer to the
     next object to be a corner case. This test could be updated to consider in == limit as a
     corner case. */
  Insist(in > testobj);

  /* Do Test */
  res = mps_addr_object(&out, arena, in);
  Insist(out == testobj);
  Insist(res == MPS_RES_OK);
  printf("Interior pointer input: passed\n");


  /* TEST 2: Test using the base pointer itself as an input*/

  in = testobj;

  /* Do Test */
  res = mps_addr_object(&out, arena, in);
  Insist(out == testobj);
  Insist(res == MPS_RES_OK);
  printf("Base pointer input: passed\n");



   /* TEST 3: Test using a pointer one-off-the-end of the object*/

  in =  (mps_addr_t)((char *)testobj + (N_SLOT_TESTOBJ + 2) * sizeof(mps_word_t));

  /* Do Test */
  res = mps_addr_object(&out, arena, in);
  Insist(res == MPS_RES_FAIL);
  printf("Pointer to next object input: passed\n");


  /* Clean up from above tests */
  mps_root_destroy(testobj_root);
  mps_ap_destroy(obj_ap);
  mps_pool_destroy(amcz_pool);
  mps_fmt_destroy(obj_fmt);


  /* TEST 4: Test using a pointer in unmanaged memory */

  /* Use malloc to allocate non-mps-managed memory on the heap */
  in = malloc(sizeof(mps_word_t));
  Insist(NULL != in);

  /* Do the test */
  res = mps_addr_object(&out, arena, in);

  /* Expect MPS to fail to find a base pointer for addresses not in managed memory */
  Insist(res == MPS_RES_FAIL);
  printf("Pointer to unmanaged memory input: passed\n");

  /* clean up from this test */
  if (NULL != in)
    free(in);


  /* TEST 5: Test using a pointer in a pool which currently doesn't implement mps_addr_object */

  /* Create mvff pool for which mps_addr_object is not implemented */
  die(mps_pool_create_k(&mvff_pool, arena, mps_class_mvff(), mps_args_none), "mps_pool_create_k mvff");

  /* allocate an object (just some memory) in this pool */
  die(mps_alloc(&in, mvff_pool, sizeof(mps_word_t)), "mps_alloc");

  /* Do the test */
  res = mps_addr_object(&out, arena, in);

  Insist(res == MPS_RES_UNIMPL);
  printf("Pointer to object in pool where mps_addr_object not implemented: passed\n");


  /* If more tests are added here, briefly describe them above under "INTRO TO TESTS" comment */

  /* Final clean up */
  mps_free(mvff_pool, in, sizeof(mps_word_t));
  mps_pool_destroy(mvff_pool);
  mps_arena_destroy(arena);
}

int main(int argc, char *argv[])
{
  testlib_init(argc, argv);

  test_main();

  printf("%s: Conclusion, failed to find any defects.\n", argv[0]);

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
