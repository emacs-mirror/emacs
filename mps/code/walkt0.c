/* walkt0.c: WALK TEST 0
 *
 * $Id$
 * Copyright (c) 1998-2020 Ravenbrook Limited.  See end of file for license.
 *
 * Loosely based on <code/steptest.c>.
 */

#include "fmtdy.h"
#include "fmtdytst.h"
#include "testlib.h"
#include "mpslib.h"
#include "mpscamc.h"
#include "mpscams.h"
#include "mpscawl.h"
#include "mpsclo.h"
#include "mpscsnc.h"
#include "mpsavm.h"
#include "mps.h"
#include "mpm.h"

#include <stdio.h> /* printf */

#define testArenaSIZE     ((size_t)((size_t)64 << 20))
#define avLEN             3
#define exactRootsCOUNT   200
#define objCOUNT          20000

#define genCOUNT          3
#define gen1SIZE          750  /* kB */
#define gen2SIZE          2000 /* kB */
#define gen3SIZE          5000 /* kB */
#define gen1MORTALITY     0.85
#define gen2MORTALITY     0.60
#define gen3MORTALITY     0.40

/* testChain -- generation parameters for the test */

static mps_gen_param_s testChain[genCOUNT] = {
    {gen1SIZE, gen1MORTALITY},
    {gen2SIZE, gen2MORTALITY},
    {gen3SIZE, gen3MORTALITY},
};

/* objNULL needs to be odd so that it's ignored in exactRoots. */
#define objNULL           ((mps_addr_t)MPS_WORD_CONST(0xDECEA5ED))

static mps_ap_t ap;
static mps_addr_t exactRoots[exactRootsCOUNT];
static size_t alloc_bytes;

/* Make a single Dylan object */

static mps_addr_t make(void)
{
    size_t length = rnd() % (avLEN * 2);
    size_t size = (length+2) * sizeof(mps_word_t);
    mps_addr_t p;
    mps_res_t res;

    alloc_bytes += size;

    for(;;) {
        mps_bool_t commit_res;
        MPS_RESERVE_BLOCK(res, p, ap, size);
        if(res)
            die(res, "MPS_RESERVE_BLOCK");
        res = dylan_init(p, size, exactRoots, exactRootsCOUNT);
        if(res)
            die(res, "dylan_init");
        commit_res = mps_commit(ap, p, size);
        if(commit_res)
            break;
    }

    return p;
}

/* A formatted objects stepper function. Passed to
 * mps_arena_formatted_objects_walk.
 *
 * Tests the (pool, format) values that MPS passes to it for each
 * object, by...
 *
 * ...1: making explicit queries with:
 *   mps_arena_has_addr
 *   mps_addr_pool
 *   mps_addr_fmt
 *
 * ...2: comparing with what we expect for:
 *   pool
 *   fmt
 *
 * ...3: accumulating the count and size of objects found
 */
typedef struct object_stepper_data {
  mps_arena_t arena;
  mps_pool_t expect_pool;
  mps_fmt_t expect_fmt;
  size_t count;                 /* number of non-padding objects found */
  size_t objSize;               /* total size of non-padding objects */
  size_t padSize;               /* total size of padding objects */
} object_stepper_data_s, *object_stepper_data_t;

static void object_stepper(mps_addr_t object, mps_fmt_t format,
                           mps_pool_t pool, void *p, size_t s)
{
    object_stepper_data_t sd;
    mps_arena_t arena;
    mps_bool_t b;
    mps_pool_t query_pool;
    mps_fmt_t query_fmt;
    size_t size;

    Insist(s == sizeof *sd);
    sd = p;
    arena = sd->arena;

    Insist(mps_arena_has_addr(arena, object));

    b = mps_addr_pool(&query_pool, arena, object);
    Insist(b);
    Insist(query_pool == pool);
    Insist(pool == sd->expect_pool);

    b = mps_addr_fmt(&query_fmt, arena, object);
    Insist(b);
    Insist(query_fmt == format);
    Insist(format == sd->expect_fmt);

    size = AddrOffset(object, dylan_skip(object));
    if (dylan_ispad(object)) {
      sd->padSize += size;
    } else {
      ++ sd->count;
      sd->objSize += size;
    }
}


/* A roots stepper function. Passed to mps_arena_roots_walk. */

typedef struct roots_stepper_data {
  mps_root_t exactRoot;
  size_t count;
} roots_stepper_data_s, *roots_stepper_data_t;

static void roots_stepper(mps_addr_t *ref, mps_root_t root, void *p, size_t s)
{
  roots_stepper_data_t data = p;
  Insist(ref != NULL);
  Insist(p != NULL);
  Insist(s == sizeof *data);
  Insist(root == data->exactRoot);
  ++ data->count;
}


/* test -- the body of the test */

static void test(mps_arena_t arena, mps_pool_class_t pool_class)
{
    mps_chain_t chain;
    mps_fmt_t format;
    mps_pool_t pool;
    mps_root_t exactRoot;
    size_t i;
    size_t totalSize, freeSize, allocSize, bufferSize;
    unsigned long objs;
    object_stepper_data_s objectStepperData, *sd;
    roots_stepper_data_s rootsStepperData, *rsd;

    die(dylan_fmt(&format, arena), "fmt_create");
    die(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

    MPS_ARGS_BEGIN(args) {
        MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format);
        MPS_ARGS_ADD(args, MPS_KEY_CHAIN, chain);
        die(mps_pool_create_k(&pool, arena, pool_class, args), "pool_create");
    } MPS_ARGS_END(args);

    die(mps_ap_create(&ap, pool, mps_rank_exact()), "ap_create");

    for(i = 0; i < exactRootsCOUNT; ++i)
        exactRoots[i] = objNULL;

    die(mps_root_create_table_masked(&exactRoot, arena,
                                     mps_rank_exact(), (mps_rm_t)0,
                                     &exactRoots[0], exactRootsCOUNT,
                                     (mps_word_t)1),
        "root_create_table(exact)");

    objs = 0;

    while(objs < objCOUNT) {
        size_t r;

        r = objs;
        i = r % exactRootsCOUNT;
        if(exactRoots[i] != objNULL) {
            cdie(dylan_check(exactRoots[i]), "dying root check");
        }
        exactRoots[i] = make();
        if(exactRoots[(exactRootsCOUNT-1) - i] != objNULL)
            dylan_write(exactRoots[(exactRootsCOUNT-1) - i],
                        exactRoots, exactRootsCOUNT);

        ++objs;
    }

    mps_arena_park(arena);

    rsd = &rootsStepperData;
    rsd->exactRoot = exactRoot;
    rsd->count = 0;
    mps_arena_roots_walk(arena, roots_stepper, rsd, sizeof *rsd);
    printf("%lu %lu\n", (unsigned long)rsd->count, (unsigned long)exactRootsCOUNT);
    Insist(rsd->count == exactRootsCOUNT);

    sd = &objectStepperData;
    sd->arena = arena;
    sd->expect_pool = pool;
    sd->expect_fmt = format;
    sd->count = 0;
    sd->objSize = 0;
    sd->padSize = 0;
    mps_arena_formatted_objects_walk(arena, object_stepper, sd, sizeof *sd);
    Insist(sd->count == objs);

    totalSize = mps_pool_total_size(pool);
    freeSize = mps_pool_free_size(pool);
    allocSize = totalSize - freeSize;
    bufferSize = AddrOffset(ap->init, ap->limit);
    printf("%s: obj=%lu pad=%lu total=%lu free=%lu alloc=%lu buffer=%lu\n",
           ClassName(pool_class),
           (unsigned long)sd->objSize,
           (unsigned long)sd->padSize,
           (unsigned long)totalSize,
           (unsigned long)freeSize,
           (unsigned long)allocSize,
           (unsigned long)bufferSize);
    Insist(sd->objSize + sd->padSize + bufferSize == allocSize);

    mps_ap_destroy(ap);
    mps_root_destroy(exactRoot);
    mps_pool_destroy(pool);
    mps_chain_destroy(chain);
    mps_fmt_destroy(format);
    mps_arena_release(arena);
}

int main(int argc, char *argv[])
{
    mps_arena_t arena;
    mps_thr_t thread;

    testlib_init(argc, argv);

    die(mps_arena_create(&arena, mps_arena_class_vm(),
                         testArenaSIZE),
        "arena_create");
    die(mps_thread_reg(&thread, arena), "thread_reg");

    test(arena, mps_class_amc());
    test(arena, mps_class_amcz());
    test(arena, mps_class_ams());
    test(arena, mps_class_awl());
    test(arena, mps_class_lo());
    test(arena, mps_class_snc());

    mps_thread_dereg(thread);
    mps_arena_destroy(arena);

    printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
    return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 1998-2020 Ravenbrook Limited <http://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
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
