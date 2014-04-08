/* gcbench.c -- "GC" Benchmark on ANSI C library
 *
 * $Id$
 * Copyright 2014 Ravenbrook Limited.  See end of file for license.
 *
 * This is an allocation stress benchmark test for gc pools
 */

#include "mps.c"
#include "getopt.h"
#include "testlib.h"
#include "testthr.h"
#include "fmtdy.h"
#include "fmtdytst.h"

#define RESMUST(expr) \
  do { \
    mps_res_t res = (expr); \
    if (res != MPS_RES_OK) { \
      fprintf(stderr, #expr " returned %d\n", res); \
      exit(EXIT_FAILURE); \
    } \
  } while(0)

static mps_arena_t arena;
static mps_pool_t pool;
static mps_fmt_t format;
static mps_chain_t chain;

/* objNULL needs to be odd so that it's ignored in exactRoots. */
#define objNULL           ((obj_t)MPS_WORD_CONST(0xDECEA5ED))
#define genLIMIT  100

static rnd_state_t seed = 0;      /* random number seed */
static unsigned nthreads = 1;     /* threads */
static unsigned niter = 5;        /* iterations */
static unsigned npass = 10;       /* passes over tree */
static size_t width = 2;          /* width of tree nodes */
static unsigned depth = 20;       /* depth of tree */
static double preuse = 0.2;       /* probability of reuse */
static double pupdate = 0.1;      /* probability of update */
static unsigned ngen = 0;         /* number of generations specified */
static mps_gen_param_s gen[genLIMIT]; /* generation parameters */
static size_t arenasize = 256ul * 1024 * 1024; /* arena size */
static unsigned pinleaf = FALSE;  /* are leaf objects pinned at start */
static mps_bool_t zoned = TRUE;   /* arena allocates using zones */

typedef struct gcthread_s *gcthread_t;

typedef void *(*gcthread_fn_t)(gcthread_t thread);

struct gcthread_s {
    testthr_t thread;
    mps_thr_t mps_thread;
    mps_root_t reg_root;
    mps_ap_t ap;
    gcthread_fn_t fn;
};

typedef mps_word_t obj_t;

static obj_t mkvector(mps_ap_t ap, size_t n) {
  mps_word_t v;
  RESMUST(make_dylan_vector(&v, ap, n));
  return v;
}

static obj_t aref(obj_t v, size_t i) {
  return DYLAN_VECTOR_SLOT(v, i);
}

static void aset(obj_t v, size_t i, obj_t val) {
  DYLAN_VECTOR_SLOT(v, i) = val;
}

/* mktree - make a tree of nodes with depth d. */
static obj_t mktree(mps_ap_t ap, unsigned d, obj_t leaf) {
  obj_t tree;
  size_t i;
  if (d <= 0) return leaf;
  tree = mkvector(ap, width);
  for (i = 0; i < width; ++i) {
    aset(tree, i, mktree(ap, d - 1, leaf));
  }
  return tree;
}

static obj_t random_subtree(obj_t tree, unsigned levels) {
  while(tree != objNULL && levels > 0) {
    tree = aref(tree, rnd() % width);
    --levels;
  }
  return tree;
}

/* new_tree - Make a new tree from an old tree.
 * The new tree is the same depth as the old tree and
 * reuses old nodes with probability preuse.
 * NOTE: If a new node is reused multiple times, the total size
 * will be smaller.
 * NOTE: Changing preuse will dramatically change how much work
 * is done.  In particular, if preuse==1, the old tree is returned
 * unchanged. */
static obj_t new_tree(mps_ap_t ap, obj_t oldtree, unsigned d) {
  obj_t subtree;
  size_t i;
  if (rnd_double() < preuse) {
    subtree = random_subtree(oldtree, depth - d);
  } else {
    if (d == 0)
      return objNULL;
    subtree = mkvector(ap, width);
    for (i = 0; i < width; ++i) {
      aset(subtree, i, new_tree(ap, oldtree, d - 1));
    }
  }
  return subtree;
}

/* Update tree to be identical tree but with nodes reallocated
 * with probability pupdate.  This avoids writing to vector slots
 * if unecessary. */
static obj_t update_tree(mps_ap_t ap, obj_t oldtree, unsigned d) {
  obj_t tree;
  size_t i;
  if (oldtree == objNULL || d == 0)
    return oldtree;
  if (rnd_double() < pupdate) {
    tree = mkvector(ap, width);
    for (i = 0; i < width; ++i) {
      aset(tree, i, update_tree(ap, aref(oldtree, i), d - 1));
    }
  } else {
    tree = oldtree;
    for (i = 0; i < width; ++i) {
      obj_t oldsubtree = aref(oldtree, i);
      obj_t subtree = update_tree(ap, oldsubtree, d - 1);
      if (subtree != oldsubtree) {
        aset(tree, i, subtree);
      }
    }
  }
  return tree;
}

static void *gc_tree(gcthread_t thread) {
  unsigned i, j;
  mps_ap_t ap = thread->ap;
  obj_t leaf = pinleaf ? mktree(ap, 1, objNULL) : objNULL;
  for (i = 0; i < niter; ++i) {
    obj_t tree = mktree(ap, depth, leaf);
    for (j = 0 ; j < npass; ++j) {
      if (preuse < 1.0)
        tree = new_tree(ap, tree, depth);
      if (pupdate > 0.0)
        tree = update_tree(ap, tree, depth);
    }
  }
  return NULL;
}

/* start -- start routine for each thread */
static void *start(void *p) {
  gcthread_t thread = p;
  void *marker;
  RESMUST(mps_thread_reg(&thread->mps_thread, arena));
  RESMUST(mps_root_create_reg(&thread->reg_root, arena, 
                              mps_rank_ambig(), (mps_rm_t)0,
                              thread->mps_thread, &mps_stack_scan_ambig,
                              &marker, (size_t)0));
  RESMUST(mps_ap_create_k(&thread->ap, pool, mps_args_none));
  thread->fn(thread);
  mps_ap_destroy(thread->ap);
  mps_root_destroy(thread->reg_root);
  mps_thread_dereg(thread->mps_thread);
  return NULL;
}

static void weave(gcthread_fn_t fn)
{
  gcthread_t threads = alloca(sizeof(threads[0]) * nthreads);
  unsigned t;
  
  for (t = 0; t < nthreads; ++t) {
    gcthread_t thread = &threads[t];
    thread->fn = fn;
    testthr_create(&thread->thread, start, thread);
  }
  
  for (t = 0; t < nthreads; ++t)
    testthr_join(&threads[t].thread, NULL);
}

static void weave1(gcthread_fn_t fn)
{
  gcthread_t thread = alloca(sizeof(thread[0]));
  
  thread->fn = fn;
  start(thread);
}


static void watch(gcthread_fn_t fn, const char *name)
{
  clock_t begin, end;
  
  begin = clock();
  if (nthreads == 1)
    weave1(fn);
  else
    weave(fn);
  end = clock();
  
  printf("%s: %g\n", name, (double)(end - begin) / CLOCKS_PER_SEC);
}


/* Setup MPS arena and call benchmark. */

static void arena_setup(gcthread_fn_t fn,
                        mps_class_t pool_class,
                        const char *name)
{
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, arenasize);
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_ZONED, zoned);
    RESMUST(mps_arena_create_k(&arena, mps_arena_class_vm(), args));
  } MPS_ARGS_END(args);
  RESMUST(dylan_fmt(&format, arena));
  /* Make wrappers now to avoid race condition. */
  /* dylan_make_wrappers() uses malloc. */
  RESMUST(dylan_make_wrappers());
  if (ngen > 0)
    RESMUST(mps_chain_create(&chain, arena, ngen, gen));
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format);
    if (ngen > 0)
      MPS_ARGS_ADD(args, MPS_KEY_CHAIN, chain);
    RESMUST(mps_pool_create_k(&pool, arena, pool_class, args));
  } MPS_ARGS_END(args);
  watch(fn, name);
  mps_pool_destroy(pool);
  mps_fmt_destroy(format);
  if (ngen > 0)
    mps_chain_destroy(chain);
  mps_arena_destroy(arena);
}


/* Command-line options definitions.  See getopt_long(3). */

static struct option longopts[] = {
  {"help",      no_argument,        NULL,   'h'},
  {"nthreads",  required_argument,  NULL,   't'},
  {"niter",     required_argument,  NULL,   'i'},
  {"npass",     required_argument,  NULL,   'p'},
  {"gen",       required_argument,  NULL,   'g'},
  {"arena-size",required_argument,  NULL,   'm'},
  {"width",     required_argument,  NULL,   'w'},
  {"depth",     required_argument,  NULL,   'd'},
  {"preuse",    required_argument,  NULL,   'r'},
  {"pupdate",   required_argument,  NULL,   'u'},
  {"pin-leaf",  no_argument,        NULL,   'l'},
  {"seed",      required_argument,  NULL,   'x'},
  {"arena-unzoned", no_argument,    NULL,   'z'},
  {NULL,        0,                  NULL,   0}
};


static struct {
  const char *name;
  gcthread_fn_t fn;
  mps_class_t (*pool_class)(void);
} pools[] = {
  {"amc", gc_tree, mps_class_amc},
  {"ams", gc_tree, mps_class_ams},
};


/* Command-line driver */

int main(int argc, char *argv[]) {
  int ch;
  unsigned i;
  int k;

  seed = rnd_seed();
  for(k=0; k<argc; k++) {
    printf("%s", argv[k]);
    if (k + 1 < argc)
      putchar(' ');
  }
  putchar('\n');
  
  while ((ch = getopt_long(argc, argv, "ht:i:p:g:m:w:d:r:u:lx:z", longopts, NULL)) != -1)
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
    case 'g':
      if (ngen >= genLIMIT) {
        fprintf(stderr, "exceeded genLIMIT\n");
          return EXIT_FAILURE;
      }
      {
        char *p;
        size_t cap = 0;
        double mort = 0.0;
        cap = (size_t)strtoul(optarg, &p, 10);
        switch(toupper(*p)) {
        case 'G': cap *= 1024;  /* fall through */
        case 'M': cap *= 1024;  /* fall through */
        case 'K': p++; break;
        default: cap = 0; break;
        }
        if (sscanf(p, ",%lg", &mort) != 1 || cap == 0) {
          fprintf(stderr, "Bad gen format '%s'\n"
          "Each gen option has format --gen=capacity[KMG],mortality\n"
          "where capacity is a size specified in kilobytes, megabytes or gigabytes\n"
          "and mortality is a number between 0 and 1\n"
          "e.g.: --gen=500K,0.85 --gen=20M,0.45\n", optarg);
            return EXIT_FAILURE;
        }
        gen[ngen].mps_capacity = cap;
        gen[ngen].mps_mortality = mort;
        ngen++;
      }
      break;
    case 'm': {
        char *p;
        arenasize = (unsigned)strtoul(optarg, &p, 10);
        switch(toupper(*p)) {
        case 'G': arenasize *= 1024;
        case 'M': arenasize *= 1024;
        case 'K': arenasize *= 1024; break;
        case '\0': break;
        default:
          fprintf(stderr, "Bad arena size %s\n", optarg);
          return EXIT_FAILURE;
        }
      }
      break;
    case 'w':
      width = (size_t)strtoul(optarg, NULL, 10);
      break;
    case 'd':
      depth = (unsigned)strtoul(optarg, NULL, 10);
      break;
    case 'r':
      preuse = strtod(optarg, NULL);
      break;
    case 'u':
      pupdate = strtod(optarg, NULL);
      break;
    case 'l':
      pinleaf = TRUE;
      break;
    case 'x':
      seed = strtoul(optarg, NULL, 10);
      break;
    case 'z':
      zoned = FALSE;
      break;
    default:
      fprintf(stderr,
              "Usage: %s [option...] [test...]\n"
              "Options:\n"
              "  -t n, --nthreads=n\n"
              "    Launch n threads each running the test (default %u).\n"
              "  -i n, --niter=n\n"
              "    Iterate each test n times (default %u).\n"
              "  -p n, --npass=n\n"
              "    Pass over the tree n times (default %u).\n"
              "  -g c,m, --gen=c[KMG],m\n"
              "    Generation with capacity c (in Kb) and mortality m\n"
              "    Use multiple times for multiple generations.\n"
              "  -m n, --arena-size=n[KMG]?\n"
              "    Initial size of arena (default %lu).\n"
              "  -w n, --width=n\n"
              "    Width of tree nodes made (default %lu)\n",
              argv[0],
              nthreads,
              niter,
              npass,
              (unsigned long)arenasize,
              (unsigned long)width);
      fprintf(stderr,
              "  -d n, --depth=n\n"
              "    Depth of tree made (default %u)\n"
              "  -r p, --preuse=p\n"
              "    Probability of reusing a node (default %g)\n"
              "  -u p, --pupdate=p\n"
              "    Probability of updating a node (default %g)\n"
              "  -l --pin-leaf\n"
              "    Make a pinned object to use for leaves.\n"
              "  -x n, --seed=n\n"
              "    Random number seed (default from entropy)\n"
              "  -z, --arena-unzoned\n"
              "    Disable zoned allocation in the arena\n"
              "Tests:\n"
              "  amc   pool class AMC\n"
              "  ams   pool class AMS\n",
              depth,
              preuse,
              pupdate);
      return EXIT_FAILURE;
    }
  argc -= optind;
  argv += optind;

  printf("seed: %lu\n", seed);
  
  while (argc > 0) {
    for (i = 0; i < sizeof(pools) / sizeof(pools[0]); ++i)
      if (strcmp(argv[0], pools[i].name) == 0)
        goto found;
    fprintf(stderr, "unknown pool test \"%s\"\n", argv[0]);
    return EXIT_FAILURE;
  found:
    rnd_state_set(seed);
    arena_setup(pools[i].fn, pools[i].pool_class(), pools[i].name);
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
