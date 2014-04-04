/* mpsicv.c: MPSI COVERAGE TEST
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (c) 2002 Global Graphics Software.
 */

#include "testlib.h"
#include "mpslib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "mpscmv.h"
#include "fmthe.h"
#include "fmtdy.h"
#include "fmtdytst.h"
#include "mps.h"
#include "mpstd.h"
#ifdef MPS_OS_W3
#  include "mpsw3.h"
#endif
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#include <string.h>


#define exactRootsCOUNT  49
#define ambigRootsCOUNT  49
#define OBJECTS          200000
#define patternFREQ      100

/* objNULL needs to be odd so that it's ignored in exactRoots. */
#define objNULL         ((mps_addr_t)MPS_WORD_CONST(0xDECEA5ED))
#define FILLER_OBJECT_SIZE 1023

#define genCOUNT          2
static mps_gen_param_s testChain[genCOUNT] = {
  { 150, 0.85 }, { 170, 0.45 } };


static mps_pool_t amcpool;
static mps_ap_t ap;
static size_t ap_headerSIZE = 0;
/* For this ap.... */
/* Auto_header format
 *
 *   [ auto_header ][===object===]
 *   ^pMps          ^pCli
 *   <-----------sizeMps--------->
 *                  <---sizeCli-->
 *
 * Note: pMps < pCli; sizeMps > sizeCli.
 */
#define PtrMps2Cli(n) ((char*)n + ap_headerSIZE)
#define PtrCli2Mps(n) ((char*)n - ap_headerSIZE)
#define SizeMps2Cli(n) (n - ap_headerSIZE)
#define SizeCli2Mps(n) (n + ap_headerSIZE)
#define HeaderInit(pMps) do {                                          \
  if(ap_headerSIZE != 0) {                                             \
    mps_addr_t pMps_MACROCOPY = (pMps);  /* macro hygiene */           \
    ((int*)pMps_MACROCOPY)[0] = realHeader;                            \
    ((int*)pMps_MACROCOPY)[1] = 0xED0ED;                               \
  }                                                                    \
 } while(0)

static mps_addr_t exactRoots[exactRootsCOUNT];
static mps_addr_t ambigRoots[ambigRootsCOUNT];


/* Types for alignment tests */

#define hasLONG_LONG 1

#ifdef _MSC_VER
#define long_long_t __int64
#else
#define long_long_t long long
#endif

struct tdouble {
  double d;
};

struct tlong {
  long d;
};

#ifdef HAS_LONG_LONG
struct tlonglong {
  long_long_t d;
};
#endif


/* alignmentTest -- test default alignment is acceptable */

static void alignmentTest(mps_arena_t arena)
{
  mps_pool_t pool;
  void *p;
  int dummy = 0;
  size_t j, size;

  die(mps_pool_create(&pool, arena, mps_class_mv(),
      (size_t)0x1000, (size_t)1024, (size_t)16384),
      "alignment pool create");
  size = max(sizeof(double), sizeof(long));
#ifdef HAS_LONG_LONG
  size = max(size, sizeof(long_long_t));
#endif
  for(j = 0; j <= size + (size_t)1; ++j) {
    die(mps_alloc(&p, pool, size + 1), "alignment alloc");

#define access(type, p) *(type*)(p) = (type)dummy; dummy += (int)*(type*)(p);

    access(double, p);
    access(long, p);
#ifdef HAS_LONG_LONG
    access(long_long_t, p);
#endif
  }
  mps_pool_destroy(pool);
}


/* make -- allocate an object */

static mps_addr_t make(void)
{
  size_t length = rnd() % 20;
  size_t sizeCli = (length+2)*sizeof(mps_word_t);
  size_t sizeMps = SizeCli2Mps(sizeCli);
  mps_addr_t pMps, pCli;
  mps_res_t res;

  do {
    MPS_RESERVE_BLOCK(res, pMps, ap, sizeMps);
    if (res != MPS_RES_OK) die(res, "MPS_RESERVE_BLOCK");
    HeaderInit(pMps);
    pCli = PtrMps2Cli(pMps);
    res = dylan_init(pCli, sizeCli, exactRoots, exactRootsCOUNT);
    if (res != MPS_RES_OK) die(res, "dylan_init");
  } while(!mps_commit(ap, pMps, sizeMps));

  return pCli;
}


/* make_with_permit -- allocate an object, with reservoir permit */

static mps_addr_t make_with_permit(void)
{
  size_t length = rnd() % 20;
  size_t sizeCli = (length+2)*sizeof(mps_word_t);
  size_t sizeMps = SizeCli2Mps(sizeCli);
  mps_addr_t pMps, pCli;
  mps_res_t res;

  do {
    MPS_RESERVE_WITH_RESERVOIR_PERMIT_BLOCK(res, pMps, ap, sizeMps);
    if (res != MPS_RES_OK) die(res, "MPS_RESERVE_WITH_RESERVOIR_PERMIT_BLOCK");
    HeaderInit(pMps);
    pCli = PtrMps2Cli(pMps);
    res = dylan_init(pCli, sizeCli, exactRoots, exactRootsCOUNT);
    if (res != MPS_RES_OK) die(res, "dylan_init");
  } while(!mps_commit(ap, pMps, sizeMps));

  return pCli;
}


/* make_no_inline -- allocate an object, using non-inlined interface */

static mps_addr_t make_no_inline(void)
{
  size_t length = rnd() % 20;
  size_t sizeCli = (length+2)*sizeof(mps_word_t);
  size_t sizeMps = SizeCli2Mps(sizeCli);
  mps_addr_t pMps, pCli;
  mps_res_t res;

  do {
    res = (mps_reserve)(&pMps, ap, sizeMps);
    if (res != MPS_RES_OK) die(res, "(mps_reserve)");
    HeaderInit(pMps);
    pCli = PtrMps2Cli(pMps);
    res = dylan_init(pCli, sizeCli, exactRoots, exactRootsCOUNT);
    if (res != MPS_RES_OK) die(res, "dylan_init");
  } while(!(mps_commit)(ap, pMps, sizeMps));

  return pCli;
}


/* alloc_v_test -- test mps_alloc_v */

static void alloc_v_test(mps_pool_t pool, ...)
{
  void *p;
  size_t size = 32;
  va_list args;

  va_start(args, pool);
  die(mps_alloc_v(&p, pool, size, args), "alloc_v");
  va_end(args);
  mps_free(pool, p, size);
}


static void pool_create_v_test(mps_arena_t arena, ...)
{
  va_list args;

  va_start(args, arena);
  die(mps_pool_create_v(&amcpool, arena, mps_class_amc(), args),
      "pool_create_v(amc)");
  va_end(args);
}

static void ap_create_v_test(mps_pool_t pool, ...)
{
  mps_ap_t apt;
  va_list args;

  va_start(args, pool);
  die(mps_ap_create_v(&apt, pool, args), "ap_create_v");
  va_end(args);
  mps_ap_destroy(apt);
}


/* addr_pool_test
 *
 * intended to test:
 *   mps_addr_pool
 *   mps_addr_fmt
 */

static void addr_pool_test(mps_arena_t arena,
                           mps_addr_t  obj1,   /* unformatted */
                           mps_pool_t  pool1,
                           mps_addr_t  obj2,   /* formatted */
                           mps_pool_t  pool2,
                           mps_fmt_t   fmt2)
{
  /* Things we might test.  An addr might be:
   *   0- a valid reference to an MPS-managed object;
   *   1- interior pointer to an MPS-managed object;
   *   2- pointer into some other part of a Seg owned by a Pool;
   *   ^^^(mps_addr_pool returns TRUE for these)
   *   3- pointer to some MPS memory that's not a Seg;
   *   4- pointer to unmapped memory;
   *   5- pointer to memory not in any Chunk.
   *   ^^^(mps_addr_pool returns FALSE for these)
   *
   * We actually test case 0 (for both unformatted and formatted 
   * objects), and case 5.
   */

  mps_bool_t b;
  mps_addr_t addr;
  /* DISTInguished values are to observe overwrites. */
  mps_pool_t poolDistinguished = (mps_pool_t)MPS_WORD_CONST(0x000d1521);
  mps_pool_t pool = poolDistinguished;
  mps_fmt_t fmtDistinguished = (mps_fmt_t)MPS_WORD_CONST(0x000d1521);
  mps_fmt_t fmt = fmtDistinguished;

  /* 0a -- obj1 in pool1 (unformatted) */
  addr = obj1;
  pool = poolDistinguished;
  fmt = fmtDistinguished;
  b = mps_addr_pool(&pool, arena, addr);
  /* printf("b %d; pool %p; sig %lx\n", b, (void *)pool,
            b ? ((mps_word_t*)pool)[0] : (mps_word_t)0); */
  cdie(b == TRUE && pool == pool1, "mps_addr_pool 0a");
  b = mps_addr_fmt(&fmt, arena, addr);
  /* printf("b %d; fmt %p; sig %lx\n", b, (void *)fmt,
            b ? ((mps_word_t*)fmt)[0] : (mps_word_t)0); */
  cdie(b == FALSE && fmt == fmtDistinguished, "mps_addr_fmt 0a");

  /* 0b -- obj2 in pool2, with fmt2 */
  addr = obj2;
  pool = poolDistinguished;
  fmt = fmtDistinguished;
  b = mps_addr_pool(&pool, arena, addr);
  /* printf("b %d; pool %p; sig %lx\n", b, (void *)pool,
            b ? ((mps_word_t*)pool)[0] : (mps_word_t)0); */
  cdie(b == TRUE && pool == pool2, "mps_addr_pool 0b");
  b = mps_addr_fmt(&fmt, arena, addr);
  /* printf("b %d; fmt %p; sig %lx\n", b, (void *)fmt,
            b ? ((mps_word_t*)fmt)[0] : (mps_word_t)0); */
  cdie(b == TRUE && fmt == fmt2, "mps_addr_fmt 0b");

  /* 5 */
  addr = &pool;  /* point at stack, not in any chunk */
  pool = poolDistinguished;
  fmt = fmtDistinguished;
  b = mps_addr_pool(&pool, arena, addr);
  cdie(b == FALSE && pool == poolDistinguished, "mps_addr_pool 5");
  b = mps_addr_fmt(&fmt, arena, addr);
  cdie(b == FALSE && fmt == fmtDistinguished, "mps_addr_fmt 5");
}


static mps_res_t root_single(mps_ss_t ss, void *p, size_t s)
{
  testlib_unused(s);
  return mps_fix(ss, (mps_addr_t *)p);
}


/* arena_commit_test
 *
 * intended to test:
 *   MPS_RES_COMMIT_LIMIT
 *   mps_arena_commit_limit
 *   mps_arena_commit_limit_set
 *   mps_arena_committed
 *   mps_arena_reserved
 * incidentally tests:
 *   mps_alloc
 *   mps_class_mv
 *   mps_pool_create
 *   mps_pool_destroy
 */

static void arena_commit_test(mps_arena_t arena)
{
  mps_pool_t pool;
  size_t committed;
  size_t reserved;
  size_t limit;
  void *p;
  mps_res_t res;

  die(mps_pool_create(&pool, arena, mps_class_mv(),
      (size_t)0x1000, (size_t)1024, (size_t)16384),
      "commit pool create");
  limit = mps_arena_commit_limit(arena);
  committed = mps_arena_committed(arena);
  reserved = mps_arena_reserved(arena);
  cdie(reserved >= committed, "reserved < committed");
  die(mps_arena_commit_limit_set(arena, committed), "commit_limit_set before");
  do {
    res = mps_alloc(&p, pool, FILLER_OBJECT_SIZE);
  } while (res == MPS_RES_OK);
  die_expect(res, MPS_RES_COMMIT_LIMIT, "Commit limit allocation");
  die(mps_arena_commit_limit_set(arena, limit), "commit_limit_set after");
  res = mps_alloc(&p, pool, FILLER_OBJECT_SIZE);
  die_expect(res, MPS_RES_OK, "Allocation failed after raising commit_limit");
  mps_pool_destroy(pool);
}


/* reservoir_test -- Test the reservoir interface
 *
 * This has not been tuned to actually dip into the reservoir.  See
 * QA test 132 for that.
 */

#define reservoirSIZE ((size_t)128 * 1024)

static void reservoir_test(mps_arena_t arena)
{
  (void)make_with_permit();
  cdie(mps_reservoir_available(arena) == 0, "empty reservoir");
  cdie(mps_reservoir_limit(arena) == 0, "no reservoir");
  mps_reservoir_limit_set(arena, reservoirSIZE);
  cdie(mps_reservoir_limit(arena) >= reservoirSIZE, "reservoir limit set");
  cdie(mps_reservoir_available(arena) >= reservoirSIZE, "got reservoir");
  (void)make_with_permit();
  mps_reservoir_limit_set(arena, 0);
  cdie(mps_reservoir_available(arena) == 0, "empty reservoir");
  cdie(mps_reservoir_limit(arena) == 0, "no reservoir");
  (void)make_with_permit();
}


static void *test(void *arg, size_t s)
{
  mps_arena_t arena;
  mps_fmt_t format;
  mps_chain_t chain;
  mps_root_t exactRoot, ambigRoot, singleRoot, fmtRoot;
  unsigned long i;
  /* Leave arena clamped until we have allocated this many objects.
     is 0 when arena has not been clamped. */
  unsigned long clamp_until = 0;
  size_t j;
  mps_word_t collections;
  mps_pool_t mv;
  mps_addr_t alloced_obj;
  size_t asize = 32;  /* size of alloced obj */
  mps_addr_t obj;
  mps_ld_s ld;
  mps_alloc_pattern_t ramp = mps_alloc_pattern_ramp();
  size_t rampCount = 0;
  mps_res_t res;

  arena = (mps_arena_t)arg;
  testlib_unused(s);

  if (rnd() & 1) {
    printf("Using auto_header format.\n");
    die(EnsureHeaderFormat(&format, arena), "EnsureHeaderFormat");
    ap_headerSIZE = headerSIZE;  /* from fmthe.h */
  } else {
    printf("Using normal format (no implicit object header: client pointers point at start of storage).\n");
    die(dylan_fmt(&format, arena), "fmt_create");
    ap_headerSIZE = 0;
  }

  die(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

  die(mps_pool_create(&mv, arena, mps_class_mv(),
      (size_t)0x10000, (size_t)32, (size_t)0x10000),
      "pool_create(mv)");

  pool_create_v_test(arena, format, chain); /* creates amc pool */

  ap_create_v_test(amcpool);

  die(mps_ap_create(&ap, amcpool), "ap_create");

  for(j = 0; j < exactRootsCOUNT; ++j) {
    exactRoots[j] = objNULL;
  }
  for(j = 0; j < ambigRootsCOUNT; ++j) {
    ambigRoots[j] = rnd_addr();
  }

  die(mps_root_create_table_masked(&exactRoot, arena,
                                   mps_rank_exact(), (mps_rm_t)0,
                                   &exactRoots[0], exactRootsCOUNT,
                                   MPS_WORD_CONST(1)),
      "root_create_table(exact)");
  die(mps_root_create_table(&ambigRoot, arena,
                            mps_rank_ambig(), (mps_rm_t)0,
                            &ambigRoots[0], ambigRootsCOUNT),
      "root_create_table(ambig)");

  obj = objNULL;

  die(mps_root_create(&singleRoot, arena,
                      mps_rank_exact(), (mps_rm_t)0,
                      &root_single, &obj, 0),
      "root_create(single)");

  /* test non-inlined reserve/commit */
  obj = make_no_inline();

  die(mps_alloc(&alloced_obj, mv, asize), "mps_alloc");
  die(dylan_init(alloced_obj, asize, exactRoots, exactRootsCOUNT),
      "dylan_init(alloced_obj)");

  addr_pool_test(arena, alloced_obj, mv, make(), amcpool, format);

  die(mps_root_create_fmt(&fmtRoot, arena,
                          mps_rank_exact(), (mps_rm_t)0,
                          dylan_fmt_A()->scan,
                          alloced_obj,
                          (mps_addr_t)(((char*)alloced_obj)+asize)),
      "root_create_fmt");

  mps_ld_reset(&ld, arena);
  mps_ld_add(&ld, arena, obj);

  if (mps_ld_isstale(&ld, arena, obj)) {
    cdie(mps_ld_isstale_any(&ld, arena), "mps_ld_isstale_any");
    mps_ld_reset(&ld, arena);
    mps_ld_add(&ld, arena, obj);
  }

  collections = mps_collections(arena);

  for(i = 0; i < OBJECTS; ++i) {
    mps_word_t c;
    size_t r;

    c = mps_collections(arena);

    if(collections != c) {
      collections = c;
      printf("\nCollection %"PRIuLONGEST", %lu objects.\n", (ulongest_t)c, i);
      for(r = 0; r < exactRootsCOUNT; ++r) {
        cdie(exactRoots[r] == objNULL || dylan_check(exactRoots[r]),
             "all roots check");
      }
      if(collections == 1) {
        mps_arena_clamp(arena);
        clamp_until = i + 10000;
      }
      if(collections % 6 == 0) {
        mps_arena_expose(arena);
        mps_arena_release(arena);
      }
      if(collections % 6 == 3) {
        mps_arena_unsafe_expose_remember_protection(arena);
        mps_arena_unsafe_restore_protection(arena);
        mps_arena_release(arena);
      }
      if(collections % 6 == 4) {
        mps_arena_unsafe_expose_remember_protection(arena);
        mps_arena_release(arena);
      }
      if(collections % 3 == 2) {
        mps_arena_park(arena);
        mps_arena_release(arena);
      }
    }

    if(clamp_until && i >= clamp_until) {
      mps_arena_release(arena);
      clamp_until = 0;
    }

    if (rnd() % patternFREQ == 0) {
      switch(rnd() % 4) {
      case 0: /* fall through */
      case 1:
        die(mps_ap_alloc_pattern_begin(ap, ramp), "alloc_pattern_begin");
        ++rampCount;
        break;
      case 2:
        res = mps_ap_alloc_pattern_end(ap, ramp);
        cdie(rampCount > 0 ? res == MPS_RES_OK : res == MPS_RES_FAIL,
             "alloc_pattern_end");
        if (rampCount > 0) {
          --rampCount;
        }
        break;
      default:
        die(mps_ap_alloc_pattern_reset(ap), "alloc_pattern_reset");
        rampCount = 0;
        break;
      }
    }

    if (rnd() & 1) {
      exactRoots[rnd() % exactRootsCOUNT] = make();
    } else {
      ambigRoots[rnd() % ambigRootsCOUNT] = make();
    }

    r = rnd() % exactRootsCOUNT;
    if (exactRoots[r] != objNULL)  {
      cdie(dylan_check(exactRoots[r]), "random root check");
    }
  }

  arena_commit_test(arena);
  reservoir_test(arena);
  alignmentTest(arena);

  die(mps_arena_collect(arena), "collect");
  mps_arena_release(arena);

  mps_free(mv, alloced_obj, 32);
  alloc_v_test(mv);
  mps_pool_destroy(mv);
  mps_ap_destroy(ap);
  mps_root_destroy(fmtRoot);
  mps_root_destroy(singleRoot);
  mps_root_destroy(exactRoot);
  mps_root_destroy(ambigRoot);
  mps_pool_destroy(amcpool);
  mps_chain_destroy(chain);
  mps_fmt_destroy(format);

  return NULL;
}


#define TEST_ARENA_SIZE              ((size_t)16<<20)


int main(int argc, char *argv[])
{
  mps_arena_t arena;
  mps_thr_t thread;
  mps_root_t reg_root;
  void *r;
  void *marker = &marker;

  testlib_init(argc, argv);

  die(mps_arena_create(&arena, mps_arena_class_vm(), TEST_ARENA_SIZE),
      "arena_create");
  die(mps_thread_reg(&thread, arena), "thread_reg");

  die(mps_root_create_reg(&reg_root, arena,
                          mps_rank_ambig(), (mps_rm_t)0,
                          thread, &mps_stack_scan_ambig,
                          marker, (size_t)0),
      "root_create_reg");

  (mps_tramp)(&r, test, arena, 0);  /* non-inlined trampoline */
  mps_tramp(&r, test, arena, 0);
  mps_root_destroy(reg_root);
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
