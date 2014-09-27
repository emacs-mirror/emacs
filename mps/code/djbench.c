/* djbench.c -- "DJ" Benchmark on ANSI C library
 *
 * $Id$
 * Copyright 2013 Ravenbrook Limited.  See end of file for license.
 *
 * This is an allocation stress benchmark test for manual variable pools
 * and also for stdlib malloc/free (for comparison).
 *
 * It repeatedly runs over an array of blocks and allocates or frees them
 * with some probability, then frees all the remaining blocks at the end.
 * This test can be iterated.
 */

#include "mps.c"

#include "getopt.h"
#include "testlib.h"
#include "testthr.h"

#include <stdio.h> /* fprintf, stderr */
#include <stdlib.h> /* alloca, exit, EXIT_SUCCESS, EXIT_FAILURE */
#include <time.h> /* CLOCKS_PER_SEC, clock */

#define DJMUST(expr) \
  do { \
    mps_res_t res = (expr); \
    if (res != MPS_RES_OK) { \
      fprintf(stderr, #expr " returned %d\n", res); \
      exit(EXIT_FAILURE); \
    } \
  } while(0)

static mps_arena_t arena;
static mps_pool_t pool;


/* The benchmark behaviour is defined as a macro in order to give realistic
   opportunities for compiler optimisation and the intended inlining of the
   MPS functions. */

static rnd_state_t seed = 0;      /* random number seed */
static unsigned nthreads = 1;     /* threads */
static unsigned niter = 50;       /* iterations */
static unsigned npass = 100;      /* passes over blocks */
static unsigned nblocks = 64;     /* number of blocks */
static unsigned sshift = 18;      /* log2 max block size in words */
static double pact = 0.2;         /* probability per pass of acting */
static unsigned rinter = 75;      /* pass interval for recursion */
static unsigned rmax = 10;        /* maximum recursion depth */
static mps_bool_t zoned = TRUE;   /* arena allocates using zones */
static size_t arena_size = 256ul * 1024 * 1024; /* arena size */
static size_t arena_grain_size = 1; /* arena grain size */

#define DJRUN(fname, alloc, free) \
  static unsigned fname##_inner(mps_ap_t ap, unsigned depth, unsigned r) { \
    struct {void *p; size_t s;} *blocks = alloca(sizeof(blocks[0]) * nblocks); \
    unsigned j, k; \
    \
    for (k = 0; k < nblocks; ++k) { \
      blocks[k].p = NULL; \
      blocks[k].s = 0; \
    } \
    \
    for (j = 0; j < npass; ++j) { \
      for (k = 0; k < nblocks; ++k) { \
        if (rnd() % 16384 < pact * 16384) { \
          if (blocks[k].p == NULL) { \
            size_t s = rnd() % ((sizeof(void *) << (rnd() % sshift)) - 1); \
            void *p = NULL; \
            if (s > 0) \
              alloc(p, s); \
            blocks[k].p = p; \
            blocks[k].s = s; \
          } else { \
            free(blocks[k].p, blocks[k].s); \
            blocks[k].p = NULL; \
          } \
        } \
      } \
      if (rinter > 0 && depth > 0 && ++r % rinter == 0) { \
        /* putchar('>'); fflush(stdout); */ \
        r = fname##_inner(ap, depth - 1, r); \
        /* putchar('<'); fflush(stdout); */ \
      } \
    } \
    \
    for (k = 0; k < nblocks; ++k) { \
      if (blocks[k].p) { \
        free(blocks[k].p, blocks[k].s); \
        blocks[k].p = NULL; \
      } \
    } \
    return r; \
  } \
  \
  static void *fname(void *p) { \
    unsigned i; \
    mps_ap_t ap = NULL; \
    if (pool != NULL) \
      DJMUST(mps_ap_create_k(&ap, pool, mps_args_none)); \
    for (i = 0; i < niter; ++i) \
      (void)fname##_inner(ap, rmax, 0); \
    if (ap != NULL) \
      mps_ap_destroy(ap); \
    return p; \
  }


/* malloc/free benchmark */

#define MALLOC_ALLOC(p, s) do { p = malloc(s); } while(0)
#define MALLOC_FREE(p, s)  do { free(p); } while(0)

DJRUN(dj_malloc, MALLOC_ALLOC, MALLOC_FREE)


/* mps_alloc/mps_free benchmark */

#define MPS_ALLOC(p, s) do { mps_alloc(&p, pool, s); } while(0)
#define MPS_FREE(p, s)  do { mps_free(pool, p, s); } while(0)

DJRUN(dj_alloc, MPS_ALLOC, MPS_FREE)


/* reserve/free benchmark */

#define ALIGN_UP(s, a) (((s) + ((a) - 1)) & ~((a) - 1))
#define RESERVE_ALLOC(p, s) \
  do { \
    size_t _s = ALIGN_UP(s, (size_t)MPS_PF_ALIGN); \
    (void)mps_reserve(&p, ap, _s); \
    (void)mps_commit(ap, p, _s); \
  } while(0)
#define RESERVE_FREE(p, s)  do { mps_free(pool, p, s); } while(0)

DJRUN(dj_reserve, RESERVE_ALLOC, RESERVE_FREE)

typedef void *(*dj_t)(void *);

static void weave(dj_t dj)
{
  testthr_t *threads = alloca(sizeof(threads[0]) * nthreads);
  unsigned t;
  
  for (t = 0; t < nthreads; ++t)
    testthr_create(&threads[t], dj, NULL);
  
  for (t = 0; t < nthreads; ++t)
    testthr_join(&threads[t], NULL);
}


static void watch(dj_t dj, const char *name)
{
  clock_t start, finish;
  
  start = clock();
  if (nthreads == 1)
    dj(NULL);
  else
    weave(dj);
  finish = clock();
  
  printf("%s: %g\n", name, (double)(finish - start) / CLOCKS_PER_SEC);
}


/* Wrap a call to dj benchmark that doesn't require MPS setup */

static void wrap(dj_t dj, mps_pool_class_t dummy, const char *name)
{
  (void)dummy;
  pool = NULL;
  watch(dj, name);
}


/* Wrap a call to a dj benchmark that requires MPS setup */

static void arena_wrap(dj_t dj, mps_pool_class_t pool_class, const char *name)
{
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, arena_size);
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_GRAIN_SIZE, arena_grain_size);
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_ZONED, zoned);
    DJMUST(mps_arena_create_k(&arena, mps_arena_class_vm(), args));
  } MPS_ARGS_END(args);
  DJMUST(mps_pool_create_k(&pool, arena, pool_class, mps_args_none));
  watch(dj, name);
  mps_pool_destroy(pool);
  mps_arena_destroy(arena);
}


/* Command-line options definitions.  See getopt_long(3). */

static struct option longopts[] = {
  {"help",             no_argument,       NULL, 'h'},
  {"nthreads",         required_argument, NULL, 't'},
  {"niter",            required_argument, NULL, 'i'},
  {"npass",            required_argument, NULL, 'p'},
  {"nblocks",          required_argument, NULL, 'b'},
  {"sshift",           required_argument, NULL, 's'},
  {"pact",             required_argument, NULL, 'c'},
  {"rinter",           required_argument, NULL, 'r'},
  {"rmax",             required_argument, NULL, 'd'},
  {"seed",             required_argument, NULL, 'x'},
  {"arena-size",       required_argument, NULL, 'm'},
  {"arena-grain-size", required_argument, NULL, 'a'},
  {"arena-unzoned",    no_argument,       NULL, 'z'},
  {NULL,               0,                 NULL, 0  }
};


/* Test definitions. */

static mps_pool_class_t dummy_class(void)
{
  return NULL;
}

static struct {
  const char *name;
  void (*wrap)(dj_t, mps_pool_class_t, const char *name);
  dj_t dj;
  mps_pool_class_t (*pool_class)(void);
} pools[] = {
  {"mvt",   arena_wrap, dj_reserve, mps_class_mvt},
  {"mvff",  arena_wrap, dj_reserve, mps_class_mvff},
  {"mv",    arena_wrap, dj_alloc,   mps_class_mv},
  {"mvb",   arena_wrap, dj_reserve, mps_class_mv}, /* mv with buffers */
  {"an",    wrap,       dj_malloc,  dummy_class},
};


/* Command-line driver */

int main(int argc, char *argv[]) {
  int ch;
  unsigned i;

  seed = rnd_seed();
  
  while ((ch = getopt_long(argc, argv, "ht:i:p:b:s:c:r:d:m:a:x:z", longopts, NULL)) != -1)
    switch (ch) {
    case 't':
      nthreads = (unsigned)strtoul(optarg, NULL, 10);
      break;
    case 'i':
      niter = (unsigned)strtoul(optarg, NULL, 10);
      break;
    case 'p':
      npass = (unsigned)strtoul(optarg, NULL, 10);
      break;
    case 'b':
      nblocks = (unsigned)strtoul(optarg, NULL, 10);
      break;
    case 's':
      sshift = (unsigned)strtoul(optarg, NULL, 10);
      break;
    case 'c':
      pact = strtod(optarg, NULL);
      break;
    case 'r':
      rinter = (unsigned)strtoul(optarg, NULL, 10);
      break;
    case 'd':
      rmax = (unsigned)strtoul(optarg, NULL, 10);
      break;
    case 'x':
      seed = strtoul(optarg, NULL, 10);
      break;
    case 'z':
      zoned = FALSE;
      break;
    case 'm': {
        char *p;
        arena_size = (unsigned)strtoul(optarg, &p, 10);
        switch(toupper(*p)) {
        case 'G': arena_size <<= 30; break;
        case 'M': arena_size <<= 20; break;
        case 'K': arena_size <<= 10; break;
        case '\0': break;
        default:
          fprintf(stderr, "Bad arena size %s\n", optarg);
          return EXIT_FAILURE;
        }
      }
      break;
    case 'a': {
        char *p;
        arena_grain_size = (unsigned)strtoul(optarg, &p, 10);
        switch(toupper(*p)) {
        case 'G': arena_grain_size <<= 30; break;
        case 'M': arena_grain_size <<= 20; break;
        case 'K': arena_grain_size <<= 10; break;
        case '\0': break;
        default:
          fprintf(stderr, "Bad arena grain size %s\n", optarg);
          return EXIT_FAILURE;
        }
      }
      break;
    default:
      /* This is printed in parts to keep within the 509 character
         limit for string literals in portable standard C. */
      fprintf(stderr,
              "Usage: %s [option...] [test...]\n"
              "Options:\n"
              "  -m n, --arena-size=n[KMG]?\n"
              "    Initial size of arena (default %lu).\n"
              "  -g n, --arena-grain-size=n[KMG]?\n"
              "    Arena grain size (default %lu).\n"
              "  -t n, --nthreads=n\n"
              "    Launch n threads each running the test\n"
              "  -i n, --niter=n\n"
              "    Iterate each test n times (default %u).\n"
              "  -p n, --npass=n\n"
              "    Pass over the block array n times (default %u).\n"
              "  -b n, --nblocks=n\n"
              "    Length of the block array (default %u).\n"
              "  -s n, --sshift=n\n"
              "    Log2 max block size in words (default %u).\n",
              argv[0],
              (unsigned long)arena_size,
              (unsigned long)arena_grain_size,
              niter,
              npass,
              nblocks,
              sshift);
      fprintf(stderr,
              "  -c p, --pact=p\n"
              "    Probability of acting on a block (default %g).\n"
              "  -r n, --rinter=n\n"
              "    Recurse every n passes if n > 0 (default %u).\n"
              "  -d n, --rmax=n\n"
              "    Maximum recursion depth (default %u).\n"
              "  -x n, --seed=n\n"
              "    Random number seed (default from entropy).\n"
              "  -z, --arena-unzoned\n"
              "    Disabled zoned allocation in the arena\n"
              "Tests:\n"
              "  mvt   pool class MVT\n"
              "  mvff  pool class MVFF\n"
              "  mv    pool class MV\n"
              "  mvb   pool class MV with buffers\n"
              "  an    malloc\n",
              pact,
              rinter,
              rmax);
      return EXIT_FAILURE;
    }
  argc -= optind;
  argv += optind;
  
  printf("seed: %lu\n", seed);
  (void)fflush(stdout);

  while (argc > 0) {
    for (i = 0; i < NELEMS(pools); ++i)
      if (strcmp(argv[0], pools[i].name) == 0)
        goto found;
    fprintf(stderr, "unknown pool test \"%s\"\n", argv[0]);
    return EXIT_FAILURE;
  found:
    (void)mps_lib_assert_fail_install(assert_die);
    rnd_state_set(seed);
    pools[i].wrap(pools[i].dj, pools[i].pool_class(), pools[i].name);
    --argc;
    ++argv;
  }
  
  return EXIT_SUCCESS;
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
