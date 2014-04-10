/* airtest.c: AMBIGUOUS INTERIOR REFERENCE TEST
 *
 * $Id: //info.ravenbrook.com/project/mps/branch/2014-01-15/nailboard/code/fotest.c#1 $
 * Copyright (c) 2014 Ravenbrook Limited.  See end of file for license.
 *
 * .overview: This test case creates a bunch of vectors, registers
 * them for finalization, and then discards the base pointers to those
 * objects, keeping only ambiguous interior references to the vector
 * entries in the stack-allocated table s.
 *
 * .options: The test has two options:
 *
 * 'interior' is the value passed as MPS_KEY_INTERIOR when creating
 * the AMC pool. If TRUE, interior pointers must keep objects alive,
 * and so if any of these objects are finalized, the test fails. If
 * FALSE, interior pointers do not keep objects alive, so it is likely
 * that all the objects will be finalized.
 *
 * 'stack' is TRUE if the C stack is registered as a root. (If FALSE,
 * we register the table of interior pointers as an ambiguous root.)
 *
 * .fail.lii6ll: The test case passes on most platforms with
 * interior=FALSE and stack=TRUE (that is, all vectors get finalized),
 * but fails on lii6ll in variety HOT. Rather than struggle to defeat
 * the Clang optimizer, we choose not to test in this configuration.
 * In any case, the MPS does not guarantee anything about timely
 * finalization (see <manual/html/topic/finalization.html#cautions>).
 */

#include "mps.h"
#include "mpsavm.h"
#include "mpscamc.h"
#include "mpslib.h"
#include "testlib.h"
#include "fmtscheme.h"

#define OBJ_LEN (1u << 4)
#define OBJ_COUNT 10

static void test_air(int interior, int stack)
{
  size_t n_finalized = 0;
  size_t i, j;
  obj_t *s[OBJ_COUNT] = {0};
  mps_root_t root = NULL;
  if (!stack) {
    mps_addr_t *p = (void *)s;
    die(mps_root_create_table(&root, scheme_arena, mps_rank_ambig(), 0, p,
                              OBJ_COUNT), "mps_root_create_table");
  }
  mps_message_type_enable(scheme_arena, mps_message_type_finalization());
  for (j = 0; j < OBJ_COUNT; ++j) {
    obj_t n = scheme_make_integer((long)j);
    obj_t obj = scheme_make_vector(OBJ_LEN, n);
    mps_addr_t ref = obj;
    mps_finalize(scheme_arena, &ref);
    s[j] = obj->vector.vector;
  }
  for (i = 1; i < OBJ_LEN; ++i) {
    obj_t n = scheme_make_integer((long)i);
    mps_message_t msg;
    for (j = 0; j + 1 < OBJ_COUNT; ++j) {
      *++s[j] = n;
    }
    mps_arena_collect(scheme_arena);
    mps_arena_release(scheme_arena);
    if (mps_message_get(&msg, scheme_arena, mps_message_type_finalization())) {
      mps_addr_t ref;
      mps_message_finalization_ref(&ref, scheme_arena, msg);
      ++ n_finalized;
      if (interior) {
        obj_t o;
        o = ref;
        error("wrongly finalized vector %ld at %p",
              o->vector.vector[0]->integer.integer, (void *)o);
      }
    }
  }
  if (!interior && n_finalized < OBJ_COUNT) {
    error("only finalized %"PRIuLONGEST" out of %"PRIuLONGEST" vectors.",
          (ulongest_t)n_finalized, (ulongest_t)OBJ_COUNT);
  }
  if (!stack) {
    mps_root_destroy(root);
  }
}

static mps_gen_param_s obj_gen_params[] = {
  { 150, 0.85 },
  { 170, 0.45 }
};

static void test_main(int interior, int stack)
{
  mps_res_t res;
  mps_chain_t obj_chain;
  mps_fmt_t obj_fmt;
  mps_thr_t thread;
  mps_root_t reg_root = NULL;
  void *marker = &marker;

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, 1 << 20);
    MPS_ARGS_DONE(args);
    res = mps_arena_create_k(&scheme_arena, mps_arena_class_vm(), args);
  } MPS_ARGS_END(args);
  if (res != MPS_RES_OK) error("Couldn't create arena");

  res = mps_chain_create(&obj_chain, scheme_arena,
                         sizeof(obj_gen_params) / sizeof(*obj_gen_params),
                         obj_gen_params);
  if (res != MPS_RES_OK) error("Couldn't create obj chain");

  scheme_fmt(&obj_fmt);

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_CHAIN, obj_chain);
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, obj_fmt);
    MPS_ARGS_ADD(args, MPS_KEY_INTERIOR, interior);
    MPS_ARGS_DONE(args);
    die(mps_pool_create_k(&obj_pool, scheme_arena, mps_class_amc(), args),
        "mps_pool_create_k");
  } MPS_ARGS_END(args);

  res = mps_ap_create_k(&obj_ap, obj_pool, mps_args_none);
  if (res != MPS_RES_OK) error("Couldn't create obj allocation point");

  res = mps_thread_reg(&thread, scheme_arena);
  if (res != MPS_RES_OK) error("Couldn't register thread");

  if (stack) {
    res = mps_root_create_reg(&reg_root, scheme_arena, mps_rank_ambig(), 0,
                              thread, mps_stack_scan_ambig, marker, 0);
    if (res != MPS_RES_OK) error("Couldn't create root");
  }
  
  test_air(interior, stack);

  mps_arena_park(scheme_arena);
  if (stack)
    mps_root_destroy(reg_root);
  mps_thread_dereg(thread);
  mps_ap_destroy(obj_ap);
  mps_pool_destroy(obj_pool);
  mps_chain_destroy(obj_chain);
  mps_fmt_destroy(obj_fmt);
  mps_arena_destroy(scheme_arena);
}

int main(int argc, char *argv[])
{
  testlib_init(argc, argv);

  test_main(TRUE, TRUE);
  test_main(TRUE, FALSE);
  /* not test_main(FALSE, TRUE) -- see .fail.lii6ll. */
  test_main(FALSE, FALSE);

  printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (c) 2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
