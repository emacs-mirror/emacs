/* 
TEST_HEADER
 id = $HopeName$
 summary = EPDR*3 from af_six measuring fragmentation
 language = c
 link = testlib.o
 stdin = af_six
 parameters = COMLIMIT=(1024*1024*100) MIN1=8 MIN2=256 MIN3=4096 EXTENDBY=65536
OUTPUT_SPEC
 result = pass
END_HEADER
*/

#include "testlib.h"
#include "mpscepdl.h"
#include "mpsavm.h"

#define MAXOBJS (30000)
#define ALIGN 8

mps_addr_t objs[MAXOBJS];
size_t    sizes[MAXOBJS];
mps_pool_t pools[MAXOBJS];
size_t    total_size;
size_t    committed;
int       fragmentation;

double fragacc;
double commacc;
double sizeacc;
double count;

void *stackpointer;
mps_arena_t arena;

static void test(void)
{
 mps_thr_t thread;
 mps_pool_t pool1, pool2, pool3;
 log_event event;
 int id;
 char *c;
 size_t size;
 unsigned long maxcom;
 size_t avg1, avg2, avg3;

 maxcom = 0;
 fragacc = 0;
 commacc = 0;
 sizeacc = 0;
 count = 0;

 avg1 = 4*MIN1;
 avg2 = 4*MIN2;
 avg3 = 4*MIN3;

 report("avg1", "%ld", avg1);
 report("avg2", "%ld", avg2);
 report("avg3", "%ld", avg3);

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*100)),
  "create space");
 cdie(mps_arena_commit_limit_set(arena, COMLIMIT), "commit limit");
 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(
  mps_pool_create(&pool1, arena, mps_class_epdr(),
                  EXTENDBY, avg1, ALIGN),
  "create EPDR pool (small)");

 cdie(
  mps_pool_create(&pool2, arena, mps_class_epdr(),
                  EXTENDBY, avg2, ALIGN),
  "create EPDR pool (medium)");

 cdie(
  mps_pool_create(&pool3, arena, mps_class_epdr(),
                  EXTENDBY, avg3, ALIGN),
  "create EPDR pool (big)");

 committed = mps_arena_committed(arena);

 while (read_event(&event)) {
  if (event.type == EVENT_ALLOC) {
   id = event.alloc.id;
   asserts(id < MAXOBJS, "MAXOBJS too small");
   size = event.alloc.size;
   sizes[id] = size;
   if (size < MIN2) {
    pools[id] = pool1;
   } else if (size < MIN3) {
    pools[id] = pool2;
   } else {
    pools[id] = pool3;
   }
   die(mps_alloc(&objs[id], pools[id], size), "alloc");
   total_size+=sizes[id];
   c = objs[id];
   *c = 43;
  } else if (event.type == EVENT_FREE) {
   id = event.free.id;
   mps_free(pools[id], objs[id], sizes[id]);
   total_size-=sizes[id];
  }
  committed = mps_arena_committed(arena);
  if (committed > maxcom) maxcom=committed;
  fragacc += ((double) (committed-total_size))/((double) committed);
  commacc += (double) committed;
  sizeacc += (double) total_size;
  count += 1;
 }
 report("maxcom", "%ld", maxcom);
 report("fragavg", "%f", fragacc/count);
 report("fragweighted", "%f", (commacc-sizeacc)/commacc);
 report("commavg", "%f", commacc/count);
 report("sizeavg", "%f", sizeacc/count);

 mps_pool_destroy(pool1);
 mps_pool_destroy(pool2);
 mps_pool_destroy(pool3);

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
