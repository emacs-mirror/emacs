/* fotest.c: FAIL-OVER TEST
 *
 * $Id$
 * Copyright (c) 2001-2013 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * This tests fail-over behaviour in low memory situations. The MVFF
 * and MVT pool classes normally maintain their list of free blocks in
 * a Coalescing Block Structure (CBS), but if the CBS cannot handle a
 * request due to running out of memory, they fall back to a Freelist
 * (which has zero memory overhead, at some cost in performance).
 *
 * This is a white box test: it patches the class of the CBS's
 * internal block pool (MFS) with a pointer to a dummy class whose
 * alloc() method always returns ResMEMORY.
 */


#include "mpscmvff.h"
#include "mpscmvt.h"
#include "mpsavm.h"

#include "testlib.h"

#include "cbs.h"
#include "mpm.h"
#include "mpmst.h"
#include "mpmtypes.h"
#include "poolmfs.h"

#include <stdarg.h>


#define testArenaSIZE   ((((size_t)3)<<24) - 4)
#define testSetSIZE 2000
#define testLOOPS 10


/* Accessors for the CBS used to implement a pool. */

extern CBS _mps_mvff_cbs(mps_pool_t);
extern CBS _mps_mvt_cbs(mps_pool_t);


/* "OOM" pool class -- dummy alloc/free pool class whose alloc()
 * method always returns ResMEMORY */

static Res OOMAlloc(Addr *pReturn, Pool pool, Size size,
                       Bool withReservoirPermit)
{
  UNUSED(pReturn);
  UNUSED(pool);
  UNUSED(size);
  UNUSED(withReservoirPermit);
  return ResMEMORY;
}

extern PoolClass PoolClassOOM(void);
DEFINE_POOL_CLASS(OOMPoolClass, this)
{
  INHERIT_CLASS(this, AbstractAllocFreePoolClass);
  this->alloc = OOMAlloc;
}


/* make -- allocate one object */

static mps_res_t make(mps_addr_t *p, mps_ap_t ap, size_t size)
{
  mps_res_t res;

  do {
    MPS_RESERVE_BLOCK(res, *p, ap, size);
    if(res != MPS_RES_OK)
      return res;
  } while(!mps_commit(ap, *p, size));

  return MPS_RES_OK;
}


/* set_oom -- set blockPool of CBS to OOM or MFS according to argument. */

static void set_oom(CBS cbs, int oom)
{
  cbs->blockPool->class = oom ? EnsureOOMPoolClass() : PoolClassMFS();
}


/* stress -- create a pool of the requested type and allocate in it */

static mps_res_t stress(size_t (*size)(unsigned long i), mps_pool_t pool, CBS cbs)
{
  mps_res_t res = MPS_RES_OK;
  mps_ap_t ap;
  unsigned long i, k;
  int *ps[testSetSIZE];
  size_t ss[testSetSIZE];

  die(mps_ap_create(&ap, pool, mps_rank_exact()), "BufferCreate");

  /* allocate a load of objects */
  for (i=0; i<testSetSIZE; ++i) {
    ss[i] = (*size)(i);

    res = make((mps_addr_t *)&ps[i], ap, ss[i]);
    if (res != MPS_RES_OK)
      goto allocFail;
    if (ss[i] >= sizeof(ps[i]))
      *ps[i] = 1; /* Write something, so it gets swap. */
  }

  for (k=0; k<testLOOPS; ++k) {
    /* shuffle all the objects */
    for (i=0; i<testSetSIZE; ++i) {
      unsigned long j = rnd()%(testSetSIZE-i);
      void *tp;
      size_t ts;
     
      tp = ps[j]; ts = ss[j];
      ps[j] = ps[i]; ss[j] = ss[i];
      ps[i] = tp; ss[i] = ts;
    }
    /* free half of the objects */
    /* upper half, as when allocating them again we want smaller objects */
    /* see randomSize() */
    for (i=testSetSIZE/2; i<testSetSIZE; ++i) {
      mps_free(pool, (mps_addr_t)ps[i], ss[i]);
      /* if (i == testSetSIZE/2) */
      /*   PoolDescribe((Pool)pool, mps_lib_stdout); */
    }
    /* allocate some new objects */
    for (i=testSetSIZE/2; i<testSetSIZE; ++i) {
      ss[i] = (*size)(i);
      res = make((mps_addr_t *)&ps[i], ap, ss[i]);
      if (res != MPS_RES_OK)
        goto allocFail;
    }

    set_oom(cbs, rnd() % 2);
  }
  set_oom(cbs, 0);

allocFail:
  mps_ap_destroy(ap);

  return res;
}


#define max(a, b) (((a) > (b)) ? (a) : (b))

#define alignUp(w, a)       (((w) + (a) - 1) & ~((size_t)(a) - 1))


/* randomSizeAligned -- produce sizes both large and small,
 * aligned by platform alignment */

static size_t randomSizeAligned(unsigned long i)
{
  size_t maxSize = 2 * 160 * 0x2000;
  /* Reduce by a factor of 2 every 10 cycles.  Total allocation about 40 MB. */
  return alignUp(rnd() % max((maxSize >> (i / 10)), 2) + 1, MPS_PF_ALIGN);
}

int main(int argc, char *argv[])
{
  mps_arena_t arena;
  mps_pool_t pool;

  randomize(argc, argv);
  mps_lib_assert_fail_install(assert_die);

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "mps_arena_create");
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_EXTEND_BY, (64 + rnd() % 64) * 1024);
    MPS_ARGS_ADD(args, MPS_KEY_MEAN_SIZE, (1 + rnd() % 8) * 8);
    MPS_ARGS_ADD(args, MPS_KEY_ALIGN, MPS_PF_ALIGN);
    MPS_ARGS_ADD(args, MPS_KEY_MVFF_ARENA_HIGH, rnd() % 2);
    MPS_ARGS_ADD(args, MPS_KEY_MVFF_SLOT_HIGH, rnd() % 2);
    MPS_ARGS_ADD(args, MPS_KEY_MVFF_FIRST_FIT, rnd() % 2);
    MPS_ARGS_DONE(args);
    die(mps_pool_create_k(&pool, arena, mps_class_mvff(), args), "create MVFF");
  } MPS_ARGS_END(args);
  {
    CBS cbs = _mps_mvff_cbs(pool);
    die(stress(randomSizeAligned, pool, cbs), "stress MVFF");
  }
  mps_pool_destroy(pool);
  mps_arena_destroy(arena);

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "mps_arena_create");
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_MIN_SIZE, (1 + rnd() % 4) * 4);
    MPS_ARGS_ADD(args, MPS_KEY_MEAN_SIZE, (1 + rnd() % 8) * 16);
    MPS_ARGS_ADD(args, MPS_KEY_MAX_SIZE, (1 + rnd() % 4) * 1024);
    MPS_ARGS_ADD(args, MPS_KEY_MVT_RESERVE_DEPTH, (1 + rnd() % 64) * 16);
    MPS_ARGS_ADD(args, MPS_KEY_MVT_FRAG_LIMIT, (rnd() % 101) / 100.0);
    MPS_ARGS_DONE(args);
    die(mps_pool_create_k(&pool, arena, mps_class_mvt(), args), "create MVFF");
  } MPS_ARGS_END(args);
  {
    CBS cbs = _mps_mvt_cbs(pool);
    die(stress(randomSizeAligned, pool, cbs), "stress MVT");
  }
  mps_pool_destroy(pool);
  mps_arena_destroy(arena);

  printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (c) 2001-2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
