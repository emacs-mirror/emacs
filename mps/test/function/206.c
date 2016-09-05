/* 
TEST_HEADER
 id = $Id$
 summary = new MVFF allocation test
 language = c
 link = testlib.o
 parameters = QUEUES=100 ITERATIONS=1000
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
  mps_pool_create(&pool, arena, mps_class_mvff(),
                  extendBy, avgSize, align, slotHigh, arenaHigh, firstFit),
  "create MVFF pool");

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
      "corrupt at %p (%s: %x, %x, %x, %c%c%c, %lx, %lx, %i, %i)",
      queue[hd].addr,
      tdesc[kind], (int) extendBy, (int) avgSize, (int) align,
      slotHigh ? 'S' : 's', arenaHigh ? 'A' : 'a', firstFit ? 'F' : 'f',
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

 comment("%s test (%x, %x, %x, %c%c%c, %lx, %lx, %i, %i) in %.2f s",
  tdesc[kind], (int) extendBy, (int) avgSize, (int) align,
  slotHigh ? 'S' : 's', arenaHigh ? 'A' : 'a', firstFit ? 'F' : 'f',
  mins, maxs, number, iter, secs);
}

static void test(void)
{
 mps_thr_t thread;
 size_t avgSize;
 size_t align = sizeof(void*);
 unsigned long extendBy, minSize = sizeof(int), maxSize;
 int i, j, kind;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*50)), "create arena");
 cdie(mps_thread_reg(&thread, arena), "register thread");

 for (i = 0; i < 5 * 2 * 2 * 2 * 2 * 2; ++i) {
  j = i;
  slotHigh = j % 2; j /= 2;
  arenaHigh = j % 2; j /= 2;
  firstFit = j % 2; j /= 2;
  extendBy = j % 2 ? 4096 : 65536; j /= 2;
  avgSize = j % 2 ? 32 : extendBy / 2;
  maxSize = j % 2 ? 64 : extendBy; j /= 2;
  kind = j % 5; j /= 5;

  dt(kind, extendBy, avgSize, align, minSize, maxSize, QUEUES, ITERATIONS);
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
