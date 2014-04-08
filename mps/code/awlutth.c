/* awlutth.c: THREADING UNIT TEST USING POOL CLASS AWL
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * DESIGN
 *
 * .design: see <design/poolawl/#test>.*
 */

#include "mpscawl.h"
#include "mpsclo.h"
#include "mpsavm.h"
#include "fmtdy.h"
#include "testlib.h"
#include "mpslib.h"
#include "mps.h"
#include "mpstd.h"

#include <pthread.h> /* pthread_create, pthread_join */
#include <stdio.h> /* printf, puts */
#include <string.h> /* strlen */


#define testArenaSIZE     ((size_t)64<<20)
#define TABLE_SLOTS 50
#define ITERATIONS 5000
#define CHATTER 100
/* The number that a half of all numbers generated from rnd are less
 * than.  Hence, probability a-half, or P a-half */
/* see <code/testlib.h> */
#define P_A_HALF (1024uL*1024uL*1024uL - 1)     /* 2^30 - 1 */


static mps_word_t bogus_class;

#define UNINIT 0x041412ED

#define DYLAN_ALIGN 4 /* depends on value defined in fmtdy.c */


static mps_word_t wrapper_wrapper[] = {
  UNINIT,                       /* wrapper */
  UNINIT,                       /* class */
  0,                            /* Extra word */
  (mps_word_t)4<<2|2,                     /* F */
  (mps_word_t)2<<(MPS_WORD_WIDTH - 8),    /* V (version 2) */
  (mps_word_t)1<<2|1,                     /* VL */
  1                             /* patterns */
};


static mps_word_t string_wrapper[] = {
  UNINIT,                       /* wrapper */
  UNINIT,                       /* class */
  0,                            /* extra word */
  0,                            /* F */
  (mps_word_t)2<<(MPS_WORD_WIDTH - 8)|(mps_word_t)3<<3|4,   /* V */
  1                             /* VL */
};

static mps_word_t table_wrapper[] = {
  UNINIT,                       /* wrapper */
  UNINIT,                       /* class */
  0,                            /* extra word */
  (mps_word_t)1<<2|1,                     /* F */
  (mps_word_t)2<<(MPS_WORD_WIDTH - 8)|2,  /* V */
  1                             /* VL */
};


static void initialise_wrapper(mps_word_t *wrapper)
{
  wrapper[0] = (mps_word_t)&wrapper_wrapper;
  wrapper[1] = (mps_word_t)&bogus_class;
}


/* create a dylan string object (byte vector) whose contents
 * are the string s (including the terminating NUL)
 * .assume.dylan-obj */
static mps_word_t *alloc_string(const char *s, mps_ap_t ap)
{
  size_t l;
  size_t objsize;
  void *p;
  mps_word_t *object;

  l = strlen(s)+1;
  /* number of words * sizeof word */
  objsize = (2 + (l+sizeof(mps_word_t)-1)/sizeof(mps_word_t)) *
            sizeof(mps_word_t);
  objsize = (objsize + DYLAN_ALIGN-1)/DYLAN_ALIGN*DYLAN_ALIGN;
  do {
    size_t i;
    char *s2;

    die(mps_reserve(&p, ap, objsize), "Reserve Leaf\n");
    object = p;
    object[0] = (mps_word_t)string_wrapper;
    object[1] = l << 2 | 1;
    s2 = (char *)&object[2];
    for(i = 0; i < l; ++i) {
      s2[i] = s[i];
    }
  } while(!mps_commit(ap, p, objsize));
  return object;
}


/* alloc_table -- create a table with n variable slots
 *
 * .assume.dylan-obj
 */
static mps_word_t *alloc_table(unsigned long n, mps_ap_t ap)
{
  size_t objsize;
  void *p;
  mps_word_t *object;
  objsize = (3 + n) * sizeof(mps_word_t);
  objsize = (objsize + MPS_PF_ALIGN-1)/MPS_PF_ALIGN*MPS_PF_ALIGN;
  do {
    unsigned long i;

    die(mps_reserve(&p, ap, objsize), "Reserve Table\n");
    object = p;
    object[0] = (mps_word_t)table_wrapper;
    object[1] = 0;
    object[2] = n << 2 | 1;
    for(i = 0; i < n; ++i) {
      object[3+i] = 0;
    }
  } while(!mps_commit(ap, p, objsize));
  return object;
}


/* gets the nth slot from a table
 * .assume.dylan-obj
 */
static mps_word_t *table_slot(mps_word_t *table, unsigned long n)
{
  return (mps_word_t *)table[3+n];
}

/* sets the nth slot in a table
 * .assume.dylan-obj
 */
static void set_table_slot(mps_word_t *table,
                           unsigned long n, mps_word_t *p)
{
  cdie(table[0] == (mps_word_t)table_wrapper, "set_table_slot");
  table[3+n] = (mps_word_t)p;
}

/* links two tables together via their link slot
 * (1st fixed part slot)
 */
static void table_link(mps_word_t *t1, mps_word_t *t2)
{
  cdie(t1[0] == (mps_word_t)table_wrapper, "table_link 1");
  cdie(t2[0] == (mps_word_t)table_wrapper, "table_link 2");
  t1[1] = (mps_word_t)t2;
  t2[1] = (mps_word_t)t1;
}


static void test(mps_ap_t leafap, mps_ap_t exactap, mps_ap_t weakap,
                 mps_ap_t bogusap)
{
  mps_word_t *weaktable;
  mps_word_t *exacttable;
  mps_word_t *preserve[TABLE_SLOTS];    /* preserves objects in the weak */
                                        /* table by referring to them */
  unsigned long i, j;
  void *p;

  exacttable = alloc_table(TABLE_SLOTS, exactap);
  weaktable = alloc_table(TABLE_SLOTS, weakap);
  table_link(exacttable, weaktable);

  /* Leave bogusap between reserve and commit for the duration */
  die(mps_reserve(&p, bogusap, 64), "Reserve bogus");

  for(i = 0; i < TABLE_SLOTS; ++i) {
    mps_word_t *string;
    if(rnd() < P_A_HALF) {
      string = alloc_string("iamalive", leafap);
      preserve[i] = string;
    } else {
      string = alloc_string("iamdead", leafap);
      preserve[i] = 0;
    }
    set_table_slot(weaktable, i, string);
    string = alloc_string("iamexact", leafap);
    set_table_slot(exacttable, i, string);
  }

  for(j = 0; j < ITERATIONS; ++j) {
    for(i = 0; i < TABLE_SLOTS; ++i) {
      mps_word_t *string;

      string = alloc_string("spong", leafap);
      UNUSED(string);
    }
  }

  for(i = 0; i < TABLE_SLOTS; ++i) {
    if(preserve[i] == 0) {
      if(table_slot(weaktable, i)) {
        fprintf(stdout,
                "Strongly unreachable weak table entry found, "
                "slot %lu.\n",
                i);
      } else {
        if(table_slot(exacttable, i) != 0) {
          fprintf(stdout,
                  "Weak table entry deleted, but corresponding "
                  "exact table entry not deleted, slot %lu.\n",
                  i);
        }
      }
    }
  }

  (void)mps_commit(bogusap, p, 64);
  puts("A okay\n");
}


struct guff_s {
  mps_arena_t arena;
  mps_thr_t thr;
};

/* v serves two purposes:
 * A pseudo stack base for the stack root.
 * Pointer to a guff structure, which packages some values needed
 * (arena and thr mostly) */
static void *setup(void *v, size_t s)
{
  struct guff_s *guff;
  mps_arena_t arena;
  mps_pool_t leafpool;
  mps_pool_t tablepool;
  mps_fmt_t dylanfmt;
  mps_fmt_t dylanweakfmt;
  mps_ap_t leafap, exactap, weakap, bogusap;
  mps_root_t stack;
  mps_thr_t thr;

  guff = (struct guff_s *)v;
  (void)s;
  arena = guff->arena;
  thr = guff->thr;

  die(mps_root_create_reg(&stack, arena, mps_rank_ambig(), 0, thr,
                          mps_stack_scan_ambig, v, 0),
      "Root Create\n");
  die(mps_fmt_create_A(&dylanfmt, arena, dylan_fmt_A()),
      "Format Create\n");
  die(mps_fmt_create_A(&dylanweakfmt, arena, dylan_fmt_A_weak()),
      "Format Create (weak)\n");
  die(mps_pool_create(&leafpool, arena, mps_class_lo(), dylanfmt),
      "Leaf Pool Create\n");
  die(mps_pool_create(&tablepool, arena, mps_class_awl(), dylanweakfmt,
                      dylan_weak_dependent),
      "Table Pool Create\n");
  die(mps_ap_create(&leafap, leafpool, mps_rank_exact()),
      "Leaf AP Create\n");
  die(mps_ap_create(&exactap, tablepool, mps_rank_exact()),
      "Exact AP Create\n");
  die(mps_ap_create(&weakap, tablepool, mps_rank_weak()),
      "Weak AP Create\n");
  die(mps_ap_create(&bogusap, tablepool, mps_rank_exact()),
      "Bogus AP Create\n");

  test(leafap, exactap, weakap, bogusap);

  mps_ap_destroy(bogusap);
  mps_ap_destroy(weakap);
  mps_ap_destroy(exactap);
  mps_ap_destroy(leafap);
  mps_pool_destroy(tablepool);
  mps_pool_destroy(leafpool);
  mps_fmt_destroy(dylanweakfmt);
  mps_fmt_destroy(dylanfmt);
  mps_root_destroy(stack);

  return NULL;
}


static void *setup_thr(void *v)
{
  struct guff_s guff;
  mps_arena_t arena = (mps_arena_t)v;
  mps_thr_t thread;
  void *r;

  die(mps_thread_reg(&thread, arena), "thread_reg");
  guff.arena = arena;
  guff.thr = thread;
  mps_tramp(&r, setup, &guff, 0);
  mps_thread_dereg(thread);

  return r;
}


int main(int argc, char *argv[])
{
  mps_arena_t arena;
  pthread_t pthread1;

  testlib_init(argc, argv);

  initialise_wrapper(wrapper_wrapper);
  initialise_wrapper(string_wrapper);
  initialise_wrapper(table_wrapper);

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "arena_create\n");
  pthread_create(&pthread1, NULL, setup_thr, (void *)arena);
  setup_thr(arena);
  pthread_join(pthread1, NULL);
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
