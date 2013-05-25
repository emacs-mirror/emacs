/* 
TEST_HEADER
 id = $Id$
 summary = new MVFF allocation test
 language = c
 link = testlib.o
END_HEADER
*/

#include <time.h>
#include "testlib.h"
#include "mpscmvff.h"
#include "mpsavm.h"

#define MAXNUMBER 1000000

void *stackpointer;
mps_arena_t arena;

mps_bool_t slotHigh, arenaHigh, firstFit;

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
   size_t extendBy, size_t avgSize, size_t align,
   size_t mins, size_t maxs, int number, int iter)
{
 mps_pool_t pool;
 int i, hd;
 clock_t time0, time1;
 size_t size;
 int secs;

 asserts(number <= MAXNUMBER, "number too big");

 time0 = clock();
 asserts(time0 != -1, "processor time not available");

 die(
  mps_pool_create(&pool, arena, mps_class_mvff(),
                  extendBy, avgSize, align, slotHigh, arenaHigh, firstFit),
  "create EPDR pool");

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
      "corrupt at %x (%s: %x, %x, %x, %x, %x, %i, %i, %i)",
      queue[hd].addr,
      tdesc[kind], (int) extendBy, (int) avgSize, (int) align,
      (int) mins, (int) maxs, number, iter,
      slotHigh*100+arenaHigh*10+firstFit);
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
 secs=(int) 100*(time1-time0)/CLOCKS_PER_SEC;

 comment("%s test (%x, %x, %x, %x, %x, %i, %i, %i) in %i centisecs",
  tdesc[kind], (int) extendBy, (int) avgSize, (int) align,
  (int) mins, (int) maxs, number, iter,
  slotHigh*100+arenaHigh*10+firstFit, secs);
}

static void test(void)
{
 mps_thr_t thread;
 size_t mins;
 int symm;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*50)), "create arena");
 cdie(mps_thread_reg(&thread, arena), "register thread");

 mins = sizeof(int);

 for (symm = 0; symm < 8; symm++) {

 slotHigh = (symm >> 2) & 1;
 arenaHigh = (symm >> 1) & 1;
 firstFit = (symm & 1);

 dt(SEQ, 4096, 32, 8, 8, 9, 5, 1000);
 dt(RANGAP, 64, 64, 8, 8, 128, 100, 100000);

 dt(DUMMY, 4096, 32, 8, 8, 64, 1000, 1000000);
 dt(SEQ, 4096, 32, 8, 8, 64, 1000, 1000000);
 dt(RAN, 4096, 32, 8, 8, 64, 1000, 1000000);
 dt(SEQGAP, 4096, 32, 8, 8, 64, 1000, 1000000);
 dt(RANGAP, 4096, 32, 8, 8, 64, 1000, 1000000);

 dt(DUMMY, 4096, 1024, 8, 100, 132, 1000, 1000000);
 dt(SEQ, 4096, 1024, 8, 100, 132, 1000, 1000000);
 dt(RAN, 4096, 1024, 8, 100, 132, 1000, 1000000);
 dt(SEQGAP, 4096, 1024, 8, 100, 132, 1000, 1000000);
 dt(RANGAP, 4096, 1024, 8, 100, 132, 1000, 1000000);

 dt(DUMMY, 128*1024, 64*1024, 8, mins, 128*1024, 100, 10000);
 dt(SEQ, 128*1024, 64*1024, 8, mins, 128*1024, 100, 10000);
 dt(RAN, 128*1024, 64*1024, 8, mins, 128*1024, 100, 10000);
 dt(SEQGAP, 128*1024, 64*1024, 8, mins, 128*1024, 100, 10000);
 dt(RANGAP, 128*1024, 64*1024, 8, mins, 128*1024, 100, 10000);

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
