/* 
TEST_HEADER
 id = $Id$
 summary = MFS functional test  allocate and free in manual fixed small pool
 language = c
 link = testlib.o
END_HEADER
*/

#include <time.h>
#include "testlib.h"
#include "mpscmfs.h"

#define MAXNUMBER 1000000

void *stackpointer;
mps_arena_t arena;

static mps_addr_t queue[MAXNUMBER];

enum {SEQ=0, RAN=1, SEQGAP=2, RANGAP=3};
static char *tdesc[] = {"sequential", "random", 
                        "sequential gap", "random gap"};

static void dotest(int kind, size_t unitSize, size_t extendBy,
 int number, int iter)
{
 mps_pool_t pool;
 int i, hd;
 clock_t time0, time1;
 double secs;

 asserts(number <= MAXNUMBER, "number too big");
 asserts(unitSize >= sizeof(int), "unitSize too small");

 time0 = clock();
 asserts(time0 != -1, "processor time not available");

 die(
  mps_pool_create(&pool, arena, mps_class_mfs(), extendBy, unitSize),
  "create pool");

 for(hd=0; hd<number; hd++)
 {
  if (ranint(2) && (kind & 2))
  {
   queue[hd]=NULL;
  }
  else
  {
   mps_alloc(&queue[hd], pool, unitSize);
   *((int *) queue[hd])=hd;
  }
 };

 hd=-1;

 for(i=0; i<iter; i++)
 {
   if (kind & 1) hd = ranint(number);
   else {ranint(number); hd=(hd+1)%number;} /* call raninit anyway
                                               to use same time */

   asserts(queue[hd]==NULL || *((int*) queue[hd])==hd,
    "corrupt object (%s: %x, %x, %i, %i)",
    tdesc[kind], unitSize, extendBy, number, iter);

   if (queue[hd] != NULL) mps_free(pool, queue[hd], unitSize);

   if (ranint(2) && (kind & 2))
   {
    queue[hd]=NULL;
   }
   else
   {
    mps_alloc(&queue[hd], pool, unitSize);
    *((int *) queue[hd])=hd;
   }
 }

 mps_pool_destroy(pool);

 time1=clock();
 secs=(time1-time0)/(double)CLOCKS_PER_SEC;

 comment("%s test (%x, %x, %i, %i) in %.2f s",
  tdesc[kind], (int) extendBy, (int) unitSize, number, iter, secs);
}


static void test(void)
{
 mps_thr_t thread;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");
 cdie(mps_thread_reg(&thread, arena), "register thread");

 dotest(SEQ, 8, 8, 1000, 10000);
 dotest(RAN, 8, 8, 1000, 10000);
 dotest(SEQGAP, 8, 8, 1000, 10000);
 dotest(RANGAP, 8, 8, 1000, 10000);

 dotest(SEQ, 8,      8, 10000, 0);
 dotest(SEQ, 8,   4096, 10000, 0); /* 4 K */
 dotest(SEQ, 8,   8192, 10000, 0); /* 8 K */
 dotest(SEQ, 8,  16384, 10000, 0); /* 16 K */
 dotest(SEQ, 8,  32768, 10000, 0); /* 32 K */
 dotest(SEQ, 8,  65536, 10000, 0); /* 64 K */
 dotest(SEQ, 8, 131072, 10000, 0); /* 128 K */

 dotest(SEQ, 8,      8, 100000, 0);
 dotest(SEQ, 8,   4096, 100000, 0); /* 4 K */
 dotest(SEQ, 8,   8192, 100000, 0); /* 8 K */
 dotest(SEQ, 8,  16384, 100000, 0); /* 16 K */
 dotest(SEQ, 8,  32768, 100000, 0); /* 32 K */
 dotest(SEQ, 8,  65536, 100000, 0); /* 64 K */
 dotest(SEQ, 8, 131072, 100000, 0); /* 128 K */

 dotest(SEQ, 4, 4, 100, 10000);
 dotest(SEQ, 5, 5, 100, 10000);
 dotest(SEQ, 15, 15, 100, 10000);
 dotest(SEQ, 64, 64, 100, 10000);

 dotest(RAN, 4, 4, 100, 10000);
 dotest(SEQ, 64, 64, 100, 1000);

 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}
