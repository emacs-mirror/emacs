/* 
TEST_HEADER
 id = $HopeName$
 summary = MVFF from af_six measuring fragmentation
 language = c
 link = testlib.o
 stdin = af_six
 parameters = ALIGN=8 AVGSIZE=128 EXTENDBY=65536 ARENAHIGH SLOTHIGH FIRSTFIT
OUTPUT_SPEC
 result = pass
END_HEADER
*/

#include "testlib.h"
#include "mpscmvff.h"
#include "mpsavm.h"

#define MAXOBJS (30000)

mps_addr_t objs[MAXOBJS];
size_t    sizes[MAXOBJS];
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
 mps_pool_t pool;
 log_event event;
 int id;
 char *c;
 size_t size;
 unsigned long maxcom;

 maxcom = 0;
 fragacc = 0;
 commacc = 0;
 sizeacc = 0;
 count = 0;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*100)),
  "create space");
 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(
  mps_pool_create(&pool, arena, mps_class_mvff(),
                  EXTENDBY, AVGSIZE, ALIGN, ARENAHIGH, SLOTHIGH, FIRSTFIT),
  "create MVFF pool");

 committed = mps_arena_committed(arena);

 while (read_event(&event)) {
  if (event.type == EVENT_ALLOC) {
   id = event.alloc.id;
   asserts(id < MAXOBJS, "MAXOBJS too small");
   size = event.alloc.size;
   sizes[id] = size;
   mps_alloc(&objs[id], pool, size);
   total_size+=sizes[id];
   c = objs[id];
   *c = 43;
  } else if (event.type == EVENT_FREE) {
   id = event.free.id;
   mps_free(pool, objs[id], sizes[id]);
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

 mps_pool_destroy(pool);

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
