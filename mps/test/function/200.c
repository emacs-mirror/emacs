/* 
TEST_HEADER
 id = $Id$
 summary = new MV allocation test
 language = c
 link = testlib.o
 parameters = ITERATIONS=10000
END_HEADER
*/

#include <time.h>
#include "testlib.h"
#include "mpscmv.h"
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
   size_t extendBy, size_t avgSize, size_t maxSize,
   unsigned long mins, unsigned long maxs, int number, int iter)
{
 mps_pool_t pool;
 int i, hd;
 clock_t time0, time1;
 size_t size;
 double secs;

 asserts(number <= MAXNUMBER, "number too big");

 time0 = clock();
 asserts(time0 != -1, "processor time not available");

 die(
  mps_pool_create(&pool, arena, mps_class_mv(),
                  extendBy, avgSize, maxSize),
  "create MV pool");

 for(hd=0; hd<number; hd++)
 {
  size = ranrange(mins, maxs);
  if ((ranint(2) && (kind & 2)) || (kind==DUMMY))
  {
   queue[hd].addr=NULL;
  }
  else
  {
   die(mps_alloc(&queue[hd].addr, pool, size), "alloc");
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
      "corrupt at %x (%s: %x, %x, %x, %lx, %lx, %i, %i)",
      queue[hd].addr,
      tdesc[kind], (int) extendBy, (int) avgSize, (int) maxSize,
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
    die(mps_alloc(&queue[hd].addr, pool, size),"alloc");
    setobj(queue[hd].addr, size, (unsigned char) (hd%256));
    queue[hd].size = size;
   }
 }

 mps_pool_destroy(pool);

 time1=clock();
 secs=(time1-time0)/(double)CLOCKS_PER_SEC;

 comment("%s test (%x, %x, %x, %lx, %lx, %i, %i) in %.2f s",
  tdesc[kind], (int) extendBy, (int) avgSize, (int) maxSize,
  mins, maxs, number, iter, secs);
}

static void test(void)
{
 mps_thr_t thread;
 unsigned long mins;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*50)), "create arena");
 cdie(mps_thread_reg(&thread, arena), "register thread");

 mins = sizeof(int);

 dt(SEQ, 4096, 32, 64*1024, 8, 9, 5, ITERATIONS);
 dt(RANGAP, 64, 64, 64, 8, 128, 100, ITERATIONS);

 dt(DUMMY, 4096, 32, 64*1024, 8, 64, 1000, ITERATIONS);
 dt(SEQ, 4096, 32, 64*1024, 8, 64, 1000, ITERATIONS);
 dt(RAN, 4096, 32, 64*1024, 8, 64, 1000, ITERATIONS);
 dt(SEQGAP, 4096, 32, 64*1024, 8, 64, 1000, ITERATIONS);
 dt(RANGAP, 4096, 32, 64*1024, 8, 64, 1000, ITERATIONS);

 dt(DUMMY, 4096, 1024, 64*1024, 100, 132, 1000, ITERATIONS);
 dt(SEQ, 4096, 1024, 64*1024, 100, 132, 1000, ITERATIONS);
 dt(RAN, 4096, 1024, 64*1024, 100, 132, 1000, ITERATIONS);
 dt(SEQGAP, 4096, 1024, 64*1024, 100, 132, 1000, ITERATIONS);
 dt(RANGAP, 4096, 1024, 64*1024, 100, 132, 1000, ITERATIONS);

 dt(DUMMY, 128*1024, 64*1024, 6400*1024, mins, 128*1024, 100, ITERATIONS);
 dt(SEQ, 128*1024, 64*1024, 6400*1024, mins, 128*1024, 100, ITERATIONS);
 dt(RAN, 128*1024, 64*1024, 6400*1024, mins, 128*1024, 100, ITERATIONS);
 dt(SEQGAP, 128*1024, 64*1024, 6400*1024, mins, 128*1024, 100, ITERATIONS);
 dt(RANGAP, 128*1024, 64*1024, 6400*1024, mins, 128*1024, 100, ITERATIONS);

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
