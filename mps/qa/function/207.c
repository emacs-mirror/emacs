/* $HopeName: MMQA_test_function!207.c(trunk.1) $
TEST_HEADER
 summary = MVFF low-memory test
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
mps_space_t space;

mps_bool_t slotHigh, arenaHigh, firstFit;
int comments = 1;

static struct {mps_addr_t addr; size_t size;} queue[MAXNUMBER];

enum {SEQ=0, RAN=1, SEQGAP=2, RANGAP=3, DUMMY=4};
static char *tdesc[] = {"sequential", "random",
                        "sequential gap", "random gap", "dummy"};

static void setobj(mps_addr_t a, size_t size, unsigned char val)
{
 unsigned char *b;
 b = a;

 commentif(comments, "Set %x, size %x = %i", b, size, (int) val);
 while (size>0)
 {
  *b=val;
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
  mps_pool_create(&pool, space, mps_class_mvff(),
                  extendBy, avgSize, align, slotHigh, arenaHigh, firstFit),
  "create MVFF pool");

 for(hd=0; hd<number; hd++)
 {
  queue[hd].addr  = NULL;

  size = ranrange(mins, maxs);
  if ((ranint(2) && (kind & 2)) || (kind==DUMMY))
  {
  }
  else
  {
   if (MPS_RES_OK == mps_alloc(&queue[hd].addr, pool, size)) {
    commentif(comments, "Alloc: %i at %x, size %x", hd, queue[hd].addr, size);
    setobj(queue[hd].addr, size, (unsigned char) (hd%256));
    queue[hd].size = size;
   } else {
    commentif(comments, "Alloc failed: %i, size %x", hd, size);
   }
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
    commentif(comments, "Free %i at %x, size %x", hd,
     queue[hd].addr, queue[hd].size);
    mps_free(pool, queue[hd].addr, queue[hd].size);
   }
   size = ranrange(mins, maxs);

   queue[hd].addr=NULL;

   if ((ranint(2) && (kind & 2)) || (kind==DUMMY))
   {
   }
   else
   {
    if (MPS_RES_OK == mps_alloc(&queue[hd].addr, pool, size)) {
     commentif(comments, "Alloc %i at %x, size %x", hd, queue[hd].addr, size);
     setobj(queue[hd].addr, size, (unsigned char) (hd%256));
     queue[hd].size = size;
    } else {
     commentif(comments, "Alloc failed: %i, size %x", hd, size);
    }
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
 size_t comlimit;

 cdie(mps_arena_create(&space, mps_arena_class_vm(), (size_t) (1024*1024*50)), "create space");
 cdie(mps_thread_reg(&thread, space), "register thread");

 for (comlimit = 512*1024; comlimit > 0; comlimit -= 4*1024) {
  mps_arena_commit_limit_set(space, comlimit);
  report("limit", "%x", comlimit);
  symm = ranint(8);
  slotHigh = (symm >> 2) & 1;
  arenaHigh = (symm >> 1) & 1;
  firstFit = (symm & 1);

  mins = ranrange(1, 16);

  dt(RANGAP, 64*1024, 32, 8, 4*mins, 4*ranrange(mins, mins*100), 1000, 100000);
 }

 mps_thread_dereg(thread);
 mps_arena_destroy(space);
}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}
