/* walkt0.c: WALK TEST 0
 *
 * $Id$
 * Copyright (c) 1998-2014 Ravenbrook Limited.  See end of file for license.
 *
 * Loosely based on <code/steptest.c>.
 */

#include "fmtdy.h"
#include "fmtdytst.h"
#include "testlib.h"
#include "mpslib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "mpstd.h"
#ifdef MPS_OS_W3
#include "mpsw3.h"
#endif
#include "mps.h"

#include <stdlib.h>
#include <string.h>

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

/* A stepper function.  Passed to mps_arena_formatted_objects_walk.
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
 */
struct stepper_data {
  mps_arena_t arena;
  mps_pool_t expect_pool;
  mps_fmt_t expect_fmt;
  unsigned long count;
};

static void stepper(mps_addr_t object, mps_fmt_t format,
    mps_pool_t pool, void *p, size_t s)
{
    struct stepper_data *sd;
    mps_arena_t arena;
    mps_bool_t b;
    mps_pool_t query_pool;
    mps_fmt_t query_fmt;

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
    
    sd->count += 1;
    return;
}

/* test -- the body of the test */

static void *test(void *arg, size_t s)
{
    mps_arena_t arena;
    mps_chain_t chain;
    mps_fmt_t format;
    mps_pool_t pool;
    mps_root_t exactRoot;
    size_t i;
    unsigned long objs;
    struct stepper_data sdStruct, *sd;

    arena = (mps_arena_t)arg;
    (void)s; /* unused */

    die(dylan_fmt(&format, arena), "fmt_create");
    die(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

    die(mps_pool_create(&pool, arena, mps_class_amc(), format, chain),
        "pool_create(amc)");

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

    sd = &sdStruct;
    sd->arena = arena;
    sd->expect_pool = pool;
    sd->expect_fmt = format;
    sd->count = 0;
    mps_arena_formatted_objects_walk(arena, stepper, sd, sizeof *sd);
    /* Note: stepper finds more than we expect, due to pad objects */
    /* printf("stepper found %ld objs\n", sd->count); */

    mps_ap_destroy(ap);
    mps_root_destroy(exactRoot);
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

    die(mps_arena_create(&arena, mps_arena_class_vm(),
                         testArenaSIZE),
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
