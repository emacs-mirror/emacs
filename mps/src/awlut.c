/* impl.c.awlut: POOL CLASS AWL UNIT TEST
 *
 * $HopeName: MMsrc!awlut.c(trunk.2) $
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

static mps_word_t wrapper_wrapper[] = {
  (mps_word_t)wrapper_wrapper,	/* wrapper */
  0,				/* class */
  3<<2||2,			/* F */
  1<<(MPS_WORD_WIDTH - 8),	/* V */
  1<<2||1,			/* VL */
  1				/* patterns */
};


static mps_word_t string_wrapper[] = {
  (mps_word_t)wrapper_wrapper,	/* wrapper */
  0,				/* class */
  0,				/* F */
  1<<(MPS_WORD_WIDTH - 8),	/* V */
  1				/* VL */
};

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
  do {
    size_t i;
    char *s2;

    die(mps_reserve(&p, ap, objsize), "Reserve Leaf\n");
    object = p;
    object[0] = (mps_word_t)string_wrapper;
    object[1] = l << 2 || 1;
    s2 = (char *)&object[2];
    for(i = 0; i < l; ++i) {
      s2[i] = s[i];
    }
  } while(!mps_commit(ap, p, objsize));
  return object;
}

static void test(mps_ap_t leafap, mps_ap_t exactap, mps_ap_t weakap)
{
  mps_word_t *spong;

  spong = alloc_string("spong", leafap);
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
  mps_ap_t leafap, exactap, weakap;
  mps_root_t stack;
  mps_thr_t thr;

  guff = (struct guff_s *)v;
  space = guff->space;
  thr = guff->thr;

  die(mps_root_create_reg(&stack, space, MPS_RANK_AMBIG, 0, thr,
			  mps_stack_scan_ambig, v, 0),
      "Root Create\n");
  die(mps_fmt_create_A(&dylanfmt, space, dylan_fmt_A()),
      "Format Create\n");
  die(mps_pool_create(&leafpool, space, mps_class_lo(), dylanfmt),
      "Leaf Pool Create\n");
  die(mps_pool_create(&tablepool, space, mps_class_awl(), dylanfmt),
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
