/* impl.c.awlut: POOL CLASS AWL UNIT TEST
 *
 * $HopeName: MMsrc!awlut.c(trunk.7) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * READERSHIP
 *
 * Any MPS developer, any interested QA.
 *
 * DESIGN
 *
 * .design: see design.mps.poolawl.test.*
 */


#include "mps.h"
#include "mpscawl.h"
#include "mpsclo.h"
#include "fmtdy.h"
#include "testlib.h"

#include <string.h>
#include <assert.h>
#include <stdio.h>
#ifdef MPS_OS_SU
#include "ossu.h"
#endif

#define TABLE_SLOTS 50
#define ITERATIONS 5000
#define CHATTER 100
/* The number that a half of all numbers generated from rnd are less
 * than.  Hence, probability a-half, or P a-half */
/* see impl.h.testlib */
#define P_A_HALF (1024uL*1024uL*1024uL - 1)     /* 2^30 - 1 */

static mps_word_t bogus_class;


static mps_word_t wrapper_wrapper[] = {
  (mps_word_t)wrapper_wrapper,	/* wrapper */
  (mps_word_t)&bogus_class,	/* class */
  3<<2|2,			/* F */
  1<<(MPS_WORD_WIDTH - 8),	/* V */
  1<<2|1,			/* VL */
  1				/* patterns */
};


static mps_word_t string_wrapper[] = {
  (mps_word_t)wrapper_wrapper,	/* wrapper */
  (mps_word_t)&bogus_class,	/* class */
  0,				/* F */
  1<<(MPS_WORD_WIDTH - 8)|3<<3|4,	/* V */
  1				/* VL */
};

static mps_word_t table_wrapper[] = {
  (mps_word_t)wrapper_wrapper,  /* wrapper */
  (mps_word_t)&bogus_class,	/* class */
  1<<2|1,			/* F */
  1<<(MPS_WORD_WIDTH - 8)|2,	/* V */
  1				/* VL */
};

#define DYLAN_ALIGN 4 /* depends on value defined in fmtdy.c */

/* create a dylan string object (byte vector) whose contents
 * are the string s (including the terminating NUL)
 * .assume.dylan-obj */
static mps_word_t *alloc_string(char *s, mps_ap_t ap)
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

/* create a table with n variable slots
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
  assert(table[0] == (mps_word_t)table_wrapper);
  table[3+n] = (mps_word_t)p;
}

/* links two tables together via their link slot
 * (1st fixed part slot)
 */
static void table_link(mps_word_t *t1, mps_word_t *t2)
{
  assert(t1[0] == (mps_word_t)table_wrapper);
  assert(t2[0] == (mps_word_t)table_wrapper);
  t1[1] = (mps_word_t)t2;
  t2[1] = (mps_word_t)t1;
}

static void test(mps_ap_t leafap, mps_ap_t exactap, mps_ap_t weakap)
{
  mps_word_t *weaktable;
  mps_word_t *exacttable;
  mps_word_t *preserve[TABLE_SLOTS];	/* preserves objects in the weak */
				        /* table by referring to them */
  unsigned long i, j;

  exacttable = alloc_table(TABLE_SLOTS, exactap);
  weaktable = alloc_table(TABLE_SLOTS, weakap);
  table_link(exacttable, weaktable);

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

  puts("A okay\n");
}


struct guff_s {
  mps_space_t space;
  mps_thr_t thr;
};

/* v serves two purposes:
 * A pseudo stack base for the stack root.
 * Pointer to a guff structure, which packages some values needed
 * (space and thr mostly) */
static void *setup(void *v, size_t s)
{
  struct guff_s *guff;
  mps_space_t space;
  mps_pool_t leafpool;
  mps_pool_t tablepool;
  mps_fmt_t dylanfmt;
  mps_fmt_t dylanweakfmt;
  mps_ap_t leafap, exactap, weakap;
  mps_root_t stack;
  mps_thr_t thr;

  guff = (struct guff_s *)v;
  (void)s;
  space = guff->space;
  thr = guff->thr;

  die(mps_root_create_reg(&stack, space, MPS_RANK_AMBIG, 0, thr,
			  mps_stack_scan_ambig, v, 0),
      "Root Create\n");
  die(mps_fmt_create_A(&dylanfmt, space, dylan_fmt_A()),
      "Format Create\n");
  die(mps_fmt_create_A(&dylanweakfmt, space, dylan_fmt_A_weak()),
      "Format Create (weak)\n");
  die(mps_pool_create(&leafpool, space, mps_class_lo(), dylanfmt),
      "Leaf Pool Create\n");
  die(mps_pool_create(&tablepool, space, mps_class_awl(), dylanweakfmt),
      "Table Pool Create\n");
  die(mps_ap_create(&leafap, leafpool, MPS_RANK_EXACT),
      "Leaf AP Create\n");
  die(mps_ap_create(&exactap, tablepool, MPS_RANK_EXACT),
      "Exact AP Create\n");
  die(mps_ap_create(&weakap, tablepool, MPS_RANK_WEAK),
      "Weak AP Create\n");
    
  test(leafap, exactap, weakap);

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


int main(void)
{
  struct guff_s guff;
  mps_space_t space;
  mps_thr_t thread;
  void *r;

  die(mps_space_create(&space), "space_create");
  die(mps_thread_reg(&thread, space), "thread_reg");
  guff.space = space;
  guff.thr = thread;
  mps_tramp(&r, setup, &guff, 0);
  mps_thread_dereg(thread);
  mps_space_destroy(space);

  return 0;
}
