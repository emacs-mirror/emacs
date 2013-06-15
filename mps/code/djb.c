/* djb.c -- "DJ" benchmark template
 * $Id$
 *
 * This file is intended to be included in other programs that define
 *
 *   DJRUN        function to call to run the template
 *   ALLOC(p, s)  allocate block of size s and assign to p
 *   FREE(p, s)   free previously-allocated block at p of size s
 */

#include <alloca.h>
#include "testlib.h"

static void DJRUN(unsigned niter,   /* iterations */
                  unsigned npass,   /* passes over blocks */
                  unsigned nblocks, /* number of blocks */
                  unsigned sshift,  /* log2 max block size in words */
                  double prob)      /* probability per pass of acting */
{
  struct {void *p; size_t s;} *blocks = alloca(sizeof(*blocks) * nblocks);
  unsigned i, j, k;

  for (k = 0; k < nblocks; ++k) {
    blocks[k].p = NULL;
  }

  for (i = 0; i < niter; ++i) {
    for (j = 0; j < npass; ++j) {
      for (k = 0; k < nblocks; ++k) {
        if (rnd() % 16384 < prob * 16384) {
          if (blocks[k].p == NULL) {
            size_t s = rnd() % ((sizeof(void *) << (rnd() % sshift)) - 1);
            void *p = NULL;
            if (s > 0) ALLOC(p, s);
            blocks[k].p = p;
            blocks[k].s = s;
          } else {
            FREE(blocks[k].p, blocks[k].s);
            blocks[k].p = NULL;
          }
        }
      }
    }
  
    for (k = 0; k < nblocks; ++k) {
      if (blocks[k].p) {
        FREE(blocks[k].p, blocks[k].s);
        blocks[k].p = NULL;
      }
    }
  }
}
