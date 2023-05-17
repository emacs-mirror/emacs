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
 * there is a way to reliably force the MPS to move a specific object. This
 * test only examines behaviour in AMCZ and MVFF pools.
 */

#include "mps.h"
#include "testlib.h"
#include "fmtdy.h"
#include "fmtdytst.h"
#include "mpsavm.h"
#include "mpscamc.h"
#include "mpscmvff.h"
#include <stdlib.h>

/* Define an arbitrarily large object size to allocate */
#define N_SLOT_TESTOBJ 100

static void test_main(void)
{
  mps_arena_t arena;
  mps_pool_t amcz_pool, mvff_pool;
  mps_ap_t obj_ap;
  mps_fmt_t obj_fmt;
  mps_root_t testobj_root;
  mps_res_t res;
  static mps_addr_t testobj;
  mps_addr_t out, in;

  /* Create arena */
  die(mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none), "mps_arena_create_k");


  /* TEST 1: Test using an interior pointer in an object in an AMCZ pool */

  /* Use the dylan format for convenience */
  die(dylan_fmt(&obj_fmt, arena), "dylan_fmt");

  /* Create amcz pool */
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

  /* Make an arbitrary sized object, size = (N_SLOT_TESTOBJ+2) * sizeof(mps_word_t).
     (See fmtdytst.c for size calculation) */
  die(make_dylan_vector((mps_word_t *)&testobj, obj_ap, N_SLOT_TESTOBJ), "make_dylan_vector");

  /* Construct a pointer to roughly halfway inside the object */
  in = (mps_addr_t)((char *)testobj + (N_SLOT_TESTOBJ/2) * sizeof(mps_word_t));

  /* Ensure that this is an interior pointer, and not the base pointer */
  Insist(in > testobj);

  /* Do Test */
  res = mps_addr_object(&out, arena, in);
  Insist(out == testobj);
  Insist(res == MPS_RES_OK);


  /* TEST 2: Test using the base pointer itself as an input*/

  in = testobj;

  /* Do Test */
  res = mps_addr_object(&out, arena, in);
  Insist(out == testobj);
  Insist(res == MPS_RES_OK);


   /* TEST 3: Test using a pointer one-off-the-end of the object*/

  in =  (mps_addr_t)((char *)testobj + (N_SLOT_TESTOBJ + 2) * sizeof(mps_word_t));

  /* Do Test */
  res = mps_addr_object(&out, arena, in);
  Insist(res == MPS_RES_FAIL);

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

  /* We should insist MPS_RES_UNIMPL here but the Trivial Method for mps_addr_object is only reached for pools that contain objects in Segs.
     Since mvff does not, and the input address is not inside a Seg, the call is rejected by the Arena with MPS_RES_FAIL */
  Insist(res == MPS_RES_FAIL);

  /* Final clean up */
  mps_free(mvff_pool, in, sizeof(mps_word_t));
  mps_pool_destroy(mvff_pool);
  mps_arena_destroy(arena);
}

int main(int argc, char *argv[])
{
  testlib_init(argc, argv);

  test_main();

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
