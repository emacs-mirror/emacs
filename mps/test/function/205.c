/* 
TEST_HEADER
 id = $Id$
 summary = new MVT allocation test, extra deep
 language = c
 link = testlib.o
 parameters = ITERATIONS=1000
END_HEADER
*/

#include <time.h>
#include "testlib.h"
#include "mpscmvt.h"
#include "mpsavm.h"

#define MAXNUMBER 1000000

void *stackpointer;
mps_arena_t arena;

static struct {mps_addr_t addr; size_t size;} queue[MAXNUMBER];

enum {SEQ=0, RAN=1, SEQGAP=2, RANGAP=3, DUMMY=4};
static char *tdesc[] = {"sequential", "random",
                        "sequential gap", "random gap", "dummy"};

static void setobj(mps_addr_t a, size_t size, unsigned char val)
{
 unsigned char *b;
 b = a;

 while (size>0)
 {
  *b=val;
 /* comment("%p = %i", b, (int) val);
 */
  b++;
  size--;
 }
}

static mps_res_t mvt_alloc(mps_addr_t *ref, mps_ap_t ap, size_t size) {
 mps_res_t res;

 size = (size + MPS_PF_ALIGN - 1) & ~ (MPS_PF_ALIGN - 1);

 do {
  MPS_RESERVE_BLOCK(res, *ref, ap, size);
  if (res != MPS_RES_OK) return res;
 } while (!mps_commit(ap, *ref, size));

 return MPS_RES_OK;
}

static int chkobj(mps_addr_t a, size_t size, unsigned char val)
{
 unsigned char *b;
 b = a;

 while (size>0)
 {
 /* comment("%p == %i", b, (int) val);
 */
  if (*b != val) return 0;
  b++;
  size--;
 }
 return 1;
}

static void dt(int kind,
   size_t minSize, size_t avgSize, size_t maxSize,
   mps_word_t depth, mps_word_t fragLimit,
   unsigned long mins, unsigned long maxs, int number, int iter)
{
 mps_pool_t pool;
 mps_ap_t ap;
 int i, hd;
 clock_t time0, time1;
 size_t size;
 double secs;

 asserts(number <= MAXNUMBER, "number too big");

 time0 = clock();
 asserts(time0 != -1, "processor time not available");

 die(
  mps_pool_create(&pool, arena, mps_class_mvt(),
                  minSize, avgSize, maxSize, depth, fragLimit),
  "create MVT pool");

 die(mps_ap_create(&ap, pool, mps_rank_ambig()), "create ap");

 for(hd=0; hd<number; hd++)
 {
  size = ranrange(mins, maxs);
  if ((ranint(2) && (kind & 2)) || (kind==DUMMY))
  {
   queue[hd].addr=NULL;
  }
  else
  {
   die(mvt_alloc(&queue[hd].addr, ap, size), "alloc");
   setobj(queue[hd].addr, size, (unsigned char) (hd%256));
   queue[hd].size = size;
  }
 };

 hd=-1;

 for(i=0; i<iter; i++)
 {
   if (kind & 1) hd = ranint(number);
   else {ranint(number); hd=(hd+1)%number;} /* call raninit anyway
                                               to use same time */

   if (queue[hd].addr != NULL)
   {
    asserts(chkobj(queue[hd].addr, queue[hd].size, (unsigned char) (hd%256)),
      "corrupt at %x (%s: %x, %x, %x, %i, %i, %lx, %lx, %i, %i)",
      queue[hd].addr,
      tdesc[kind], (int) minSize, (int) avgSize, (int) maxSize,
      (int) depth, (int) fragLimit,
      mins, maxs, number, iter);
    mps_free(pool, queue[hd].addr, queue[hd].size);
   }
   size = ranrange(mins, maxs);

   if ((ranint(2) && (kind & 2)) || (kind==DUMMY))
   {
    queue[hd].addr=NULL;
   }
   else
   {
    die(mvt_alloc(&queue[hd].addr, ap, size),"alloc");
    setobj(queue[hd].addr, size, (unsigned char) (hd%256));
    queue[hd].size = size;
   }
 }

 mps_ap_destroy(ap);
 mps_pool_destroy(pool);

 time1=clock();
 secs=(time1-time0)/(double)CLOCKS_PER_SEC;

 comment("%s test (%x, %x, %x, %i, %i, %lx, %lx, %i, %i) in %.2f s",
  tdesc[kind], (int) minSize, (int) avgSize, (int) maxSize,
  (int) depth, (int) fragLimit,
  mins, maxs, number, iter, secs);
}

static void test(void)
{
 mps_thr_t thread;
 unsigned long mins;
 mps_word_t dep, frag;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*100)), "create arena");
 cdie(mps_thread_reg(&thread, arena), "register thread");

 mins = sizeof(int);

 dep = 10000;

 for (frag = 40; frag >= 5; frag = (frag >> 1)) {

 comment("Frag: %i", frag);

 dt(SEQ, 8, 8, 9, dep, frag, 8, 9, 5, ITERATIONS);
 dt(RANGAP, 64, 64, 64, dep, frag, 8, 128, 100, ITERATIONS);

 dt(DUMMY, 8, 32, 64, dep, frag, 8, 64, 1000, ITERATIONS);
 dt(SEQ, 8, 32, 64, dep, frag, 8, 64, 1000, ITERATIONS);
 dt(RAN, 8, 32, 64, dep, frag, 8, 64, 1000, ITERATIONS);
 dt(SEQGAP, 8, 32, 64, dep, frag, 8, 64, 1000, ITERATIONS);
 dt(RANGAP, 8, 32, 64, dep, frag, 8, 64, 1000, ITERATIONS);

 dt(DUMMY, 100, 116, 132, dep, frag, 100, 132, 1000, ITERATIONS);
 dt(SEQ, 100, 116, 132, dep, frag, 100, 132, 1000, ITERATIONS);
 dt(RAN, 100, 116, 132, dep, frag, 100, 132, 1000, ITERATIONS);
 dt(SEQGAP, 100, 116, 132, dep, frag, 100, 132, 1000, ITERATIONS);
 dt(RANGAP, 100, 116, 132, dep, frag, 100, 132, 1000, ITERATIONS);

 dt(DUMMY, mins, 60*1024, 120*1024, dep, frag, mins, 128*1024, 100, ITERATIONS);
 dt(SEQ, mins, 60*1024, 120*1024, dep, frag, mins, 128*1024, 100, ITERATIONS);
 dt(RAN, mins, 60*1024, 120*1024, dep, frag, mins, 128*1024, 100, ITERATIONS);
 dt(SEQGAP, mins, 60*1024, 120*1024, dep, frag, mins, 128*1024, 100, ITERATIONS);
 dt(RANGAP, mins, 60*1024, 120*1024, dep, frag, mins, 128*1024, 100, ITERATIONS);

/* try again using exceptional obj for anything over 16K */

 dt(DUMMY, mins, 8*1024, 16*1024, dep, frag, mins, 128*1024, 100, ITERATIONS);
 dt(SEQ, mins, 8*1024, 16*1024, dep, frag, mins, 128*1024, 100, ITERATIONS);
 dt(RAN, mins, 8*1024, 16*1024, dep, frag, mins, 128*1024, 100, ITERATIONS);
 dt(SEQGAP, mins, 8*1024, 16*1024, dep, frag, mins, 128*1024, 100, ITERATIONS);
 dt(RANGAP, mins, 8*1024, 16*1024, dep, frag, mins, 128*1024, 100, ITERATIONS);

 }

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
